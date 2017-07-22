{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Imperator.Interpreter
  ( runImperator
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.Traversable

import           Data.Default
import           Data.Default.Instances.Containers
import           Data.Map                          (Map)
import qualified Data.Map                          as Map

import           Imperator.Syntax
import           Imperator.Util                    (readDateTime, showDateTime,
                                                    xor)

instance Default ImperatorContext where
  def = ImperatorContext def

data ImperatorError = UndefinedVariable String
                    | AttemptedRedefinition String
                    | InvalidDateTimeLiteral String
                    deriving (Eq, Ord, Read, Show)

type ImperatorM a = StateT ImperatorContext (EitherT ImperatorError IO) a

data ImperatorContext = ImperatorContext { _vars :: Map String Integer
                                         }
makeLenses ''ImperatorContext

runImperator :: Statement -> IO (Either ImperatorError ())
runImperator = runEitherT . flip evalStateT def . runStatement

runStatement :: Statement -> ImperatorM ()
runStatement (Statements s) = void $ for s runStatement
runStatement (StmtDecVar _ identifier init) = use (vars . at identifier) >>= \case
  Just _  -> lift . left $ AttemptedRedefinition identifier
  Nothing -> do
    initialValue <- runVarInitializer init
    (vars . at identifier) .= Just initialValue
runStatement (StmtSetVar identifier expr) = use (vars . at identifier) >>= \case
  Nothing -> lift . left $ UndefinedVariable identifier
  Just _  -> do
    newValue <- runArithE expr
    (vars . at identifier) .= Just newValue
runStatement (StmtConditional cond tCase fCase) = do
  cond' <- runBoolE cond
  if cond' then runStatement tCase else runStatement fCase
runStatement (StmtPutS s) = liftIO $ putStrLn s
runStatement (StmtPutI i) = liftIO . putStrLn . show         =<< runArithE i
runStatement (StmtPutD d) = liftIO . putStrLn . showDateTime =<< runArithE d
runStatement (StmtPrintS s) = liftIO $ putStr s
runStatement (StmtPrintI i) = liftIO . putStr . show         =<< runArithE i
runStatement (StmtPrintD d) = liftIO . putStr . showDateTime =<< runArithE d
runStatement (StmtPrintln)  = liftIO $ putStrLn ""
runStatement (StmtNop)      = return ()

runPrompt :: String -> ImperatorM String
runPrompt p = do
  liftIO . putStr $ p ++ "> "
  liftIO $ getLine

runVarInitializer :: VarInitializer -> ImperatorM Integer
runVarInitializer (VarInitExpr expr)      = runArithE expr
runVarInitializer (VarInitPromptNumber p) = read <$> runPrompt p
runVarInitializer i@(VarInitPromptDate p) = readDateTime <$> runPrompt p >>= \case
  Nothing -> runVarInitializer i
  Just v  -> return v

runArithE :: ArithE -> ImperatorM Integer
runArithE (IntConst c) = return c
runArithE (DateTimeConst c) = case readDateTime c of
  Nothing -> lift . left $ InvalidDateTimeLiteral c
  Just v  -> return v
runArithE (VarLiteral identifier) = use (vars . at identifier) >>= \case
  Nothing -> lift . left $ UndefinedVariable identifier
  Just v  -> return v
runArithE (ArithNegate e) = negate <$> runArithE e
runArithE (ArithBinE op l r) = do
  l' <- runArithE l
  r' <- runArithE r
  return $ case op of
    ArithAdd -> l' + r'
    ArithSub -> l' - r'
    ArithMul -> l' * r'
    ArithDiv -> quot l' r'
    ArithPow -> l' ^ r'

runBoolE :: BoolE -> ImperatorM Bool
runBoolE (BoolConst b) = return b
runBoolE (BoolNegate e) = not <$> runBoolE e
runBoolE (BinBoolE op l r) = do
  l' <- runBoolE l
  r' <- runBoolE r
  return $ case op of
    BoolAnd -> l' && r'
    BoolOr  -> l' || r'
    BoolXor -> l' `xor` r'
runBoolE (ArithCmpE op l r) = do
  l' <- runArithE l
  r' <- runArithE r
  return $ case op of
    ArithGT  -> l' > r'
    ArithLT  -> l' < r'
    ArithGTE -> l' >= r'
    ArithLTE -> l' <= r'
    ArithEQ  -> l' == r'
    ArithNEQ -> l' /= r'
