module Imperator.Parser
  ( parseImperator
  ) where

import           Data.Functor.Identity                  (Identity)

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

import           Imperator.Syntax

parseImperator :: SourceName -> String -> Either ParseError Statement
parseImperator = parse statement

languageDef :: LanguageDef u
languageDef = let reservedNames   = words "var set if then else true false puts puti putd print printi printd println nop number timestamp prompt"
                  reservedOpNames = words "+ - * ** / := $= < > >= <= == != && || ^ !"
              in
                  emptyDef { Token.commentStart    = "/*"
                           , Token.commentEnd      = "*/"
                           , Token.commentLine     = "//"
                           , Token.identStart      = letter
                           , Token.identLetter     = alphaNum
                           , Token.reservedNames   = reservedNames
                           , Token.reservedOpNames = reservedOpNames
                           }

-- todo: Too lazy to fill in the type holes. Sue me.
lexer         = Token.makeTokenParser languageDef
identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
angles        = Token.angles        lexer
integer       = Token.integer       lexer
semi          = Token.semi          lexer
whiteSpace    = Token.whiteSpace    lexer
stringLiteral = Token.stringLiteral lexer
semiSep1      = Token.semiSep1      lexer
lexeme        = Token.lexeme        lexer

parseVarType :: Parser VarType
parseVarType = parseVarNumber <|> parseVarTimestamp
  where parseVarNumber    = reserved "number"    >> return VarNumber
        parseVarTimestamp = reserved "timestamp" >> return VarTimestamp

reservedRep :: String -> a -> Parser a
reservedRep r t = reserved r >> return t

reservedOpRep :: String -> a -> Parser a
reservedOpRep  r t = reservedOp r >> return t

-- todo: generalize all these (reserved x >> return SomeRepOfX), theyre so ugly
statement :: Parser Statement
statement = whiteSpace >> (parens statement <|> multipleStatements)
  where multipleStatements = do
          stmts <- semiSep1 statement' -- hack hack hack to prevent a semicolon at last statement from causing parsing to fail
          return $ if length stmts == 1 then head stmts else Statements stmts

        statement' = foldr1 (<|>) [decVarStmt, setVarStmt, ifStmt, putsStmt, putiStmt, putdStmt, printsStmt, printiStmt, printdStmt, printlnStmt, nopStmt]

        parseMaybePrompt = (reserved "prompt" >> Just <$> stringLiteral) <|> return Nothing

        decVarStmt = do
          reserved "var"
          varType <- parseVarType
          name <- identifier
          hasPrompt <- parseMaybePrompt
          let baseConstructor = StmtDecVar varType name
          case hasPrompt of
            Nothing -> baseConstructor . VarInitExpr <$> parseArithE
            Just p  -> return $ case varType of
              VarNumber    -> baseConstructor $ VarInitPromptNumber p
              VarTimestamp -> baseConstructor $ VarInitPromptDate p

        setVarStmt = reserved "set" >> (StmtSetVar <$> identifier <*> parseArithE)

        ifStmt = do
          reserved "if"
          cond <- parseBoolE
          reserved "then"
          trueCase <- statement
          reserved "else"
          falseCase <- statement
          return $ StmtConditional cond trueCase falseCase

        putsStmt    =  reserved "puts"    >> stringLiteral >>= return . StmtPutS
        putiStmt    =  reserved "puti"    >> parseArithE   >>= return . StmtPutI
        putdStmt    =  reserved "putd"    >> parseArithE   >>= return . StmtPutD
        printsStmt  =  reserved "print"   >> stringLiteral >>= return . StmtPrintS
        printiStmt  =  reserved "printi"  >> parseArithE   >>= return . StmtPrintI
        printdStmt  =  reserved "printd"  >> parseArithE   >>= return . StmtPrintD
        printlnStmt =  reservedRep "println" StmtPrintln
        nopStmt     =  reservedRep "nop"    StmtNop
                   <|> (eof >> return StmtNop) -- fix that annoying thing where you cant have a semicolon after last stmt
                   <|> (whiteSpace >> return StmtNop) -- or multiple semicolons

        parseBoolE  = buildExpressionParser boolOps boolTerm
        boolTerm    = parens parseBoolE <|> parseTrue <|> parseFalse <|> parseArithCmpE
        parseTrue   = reservedRep "true"  (BoolConst True)
        parseFalse  = reservedRep "false" (BoolConst False)
        parseArithCmpE = do
          left <- parseArithE
          op <- parseArithCmp
          right <- parseArithE
          return $ ArithCmpE op left right

        parseArithCmp   =  reservedOpRep ">"  ArithGT
                       <|> reservedOpRep "<"  ArithLT
                       <|> reservedOpRep ">=" ArithGTE
                       <|> reservedOpRep "<=" ArithLTE
                       <|> reservedOpRep "==" ArithEQ
                       <|> reservedOpRep "!=" ArithNEQ
        boolOps     = [ [ Prefix (reservedOpRep "!" BoolNegate) ]
                      , [ Infix (reservedOpRep "&&" (BinBoolE BoolAnd)) AssocLeft
                        , Infix (reservedOpRep "||" (BinBoolE BoolOr )) AssocLeft
                        , Infix (reservedOpRep "^"  (BinBoolE BoolXor)) AssocLeft
                        ]
                      ]

        parseArithE = buildExpressionParser arithOps arithTerm
        arithTerm   = parens parseArithE <|> (VarLiteral <$> identifier) <|> (IntConst <$> integer) <|> parseDateTimeExpr
        parseDateTimeExpr = angles (many $ noneOf "<>") >>= return . DateTimeConst
        arithOps    = [ [ Prefix (reservedOpRep "-" ArithNegate)
                        ]
                      , [ Infix (reservedOpRep "**" (ArithBinE ArithPow)) AssocLeft
                        ]
                      , [ Infix (reservedOpRep "*" (ArithBinE ArithMul)) AssocLeft
                        , Infix (reservedOpRep "/" (ArithBinE ArithDiv)) AssocLeft
                        ]
                      , [ Infix (reservedOpRep "+" (ArithBinE ArithAdd)) AssocLeft
                        , Infix (reservedOpRep "-" (ArithBinE ArithSub)) AssocLeft
                        ]
                      ]
