module Imperator.Syntax where

import           Imperator.Util (xor)

-- Arithmetic operators
data ArithOp = ArithAdd
             | ArithSub
             | ArithMul
             | ArithDiv
             | ArithPow
             deriving (Eq, Ord, Read, Show)

-- lower an ArithOp from its AST rep into a haskell function
lowerArithOp :: Integral n => ArithOp -> (n -> n -> n)
lowerArithOp ArithAdd = (+)
lowerArithOp ArithSub = (-)
lowerArithOp ArithMul = (*)
lowerArithOp ArithDiv = quot
lowerArithOp ArithPow = (^)

data ArithCmp = ArithGT
              | ArithLT
              | ArithEQ
              | ArithNEQ
              | ArithGTE
              | ArithLTE
              deriving (Eq, Ord, Read, Show)

-- lower an ArithCmp from its AST rep into a haskell function
lowerArithCmp :: Ord o => ArithCmp -> (o -> o -> Bool)
lowerArithCmp ArithGT  = (>)
lowerArithCmp ArithLT  = (<)
lowerArithCmp ArithEQ  = (==)
lowerArithCmp ArithNEQ = (/=)
lowerArithCmp ArithGTE = (>=)
lowerArithCmp ArithLTE = (<=)

-- Arithmetic Expression
data ArithE = VarLiteral String
            | IntConst Integer
            | ArithNegate ArithE
            | ArithBinE ArithOp ArithE ArithE
            deriving (Eq, Ord, Read, Show)

-- Binary Boolean Operation
data BoolOp = BoolAnd
            | BoolOr
            | BoolXor
            deriving (Eq, Ord, Read, Show)

-- lower a BoolOp from its AST rep into a haskell function
lowerBoolOp :: BoolOp -> (Bool -> Bool -> Bool)
lowerBoolOp BoolAnd = (&&)
lowerBoolOp BoolOr  = (||)
lowerBoolOp BoolXor = xor

-- Boolean Expression
data BoolE = BoolConst Bool
           | BoolNegate BoolE
           | BinBoolE BoolOp BoolE BoolE
           | ArithCmpE ArithCmp ArithE ArithE
           deriving (Eq, Ord, Read, Show)

-- The DSL requires us to support dates and numbers
-- Dates are just timestamps, but have a special semantic
-- meaning in the tokenizer since there needs to be a conversion
data VarType = VarNumber
             | VarTimestamp
             deriving (Eq, Ord, Read, Show)

-- For convenience, vars can be initialized through console input
-- or as a constant expression. For the same reason mentioned above,
-- we need to have semantic distinguisher between dates and ints
data VarInitializer = VarInitExpr ArithE
                    | VarInitPromptNumber String
                    | VarInitPromptDate   String
                    deriving (Eq, Ord, Read, Show)

-- A statement in the language
-- For simplicitly in the parser, a sequence of statements
-- is a statement in itself.
data Statement = StmtDecVar VarType String VarInitializer -- String is identifier
               | StmtSetVar String ArithE -- String is also identifier
               | StmtConditional BoolE Statement Statement
               | StmtPutS String
               | StmtPutI ArithE
               | StmtPutD ArithE
               | StmtPrintS String
               | StmtPrintI ArithE
               | StmtPrintD ArithE
               | StmtPrintln
               | StmtNop
               | Statements [Statement]
               deriving (Eq, Ord, Read, Show)
