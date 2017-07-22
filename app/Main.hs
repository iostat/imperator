{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad
import           System.Environment    (getArgs)
import           System.IO             (BufferMode (NoBuffering), hSetBuffering,
                                        stdin, stdout)

import           Imperator.Interpreter
import           Imperator.Parser

-- yes, im using `error` here. not like this is production code lmao
main :: IO ()
main = do
  hSetBuffering stdin  NoBuffering -- important
  hSetBuffering stdout NoBuffering -- ditto

  args <- getArgs
  when (length args /= 1) $ error "USAGE: imperator path/to/file.dsl"
  let fileName = args !! 0
  file <- readFile fileName
  case parseImperator fileName file of
    Left parseError -> error $ "Error when parsing: " ++ show parseError
    Right stmt      -> runImperator stmt >>= \case
      Left interpError -> error $ "Error while interpreting: " ++ show interpError
      Right res        -> return ()
