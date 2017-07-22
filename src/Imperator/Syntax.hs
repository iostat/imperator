module Imperator.Syntax where

-- Arithmetic operators
data ArithOp = ArithAdd
             | ArithSub
             | ArithMul
             | ArithDiv
             | ArithPow
             deriving (Eq, Ord, Read, Show)

data ArithCmp = ArithGT
              | ArithLT
              | ArithEQ
              | ArithNEQ
              | ArithGTE
              | ArithLTE
              deriving (Eq, Ord, Read, Show)

-- Arithmetic Expression
data ArithE = VarLiteral String
            | IntConst Integer
            | DateTimeConst String
            | ArithNegate ArithE
            | ArithBinE ArithOp ArithE ArithE
            deriving (Eq, Ord, Read, Show)

-- Binary Boolean Operation
data BoolOp = BoolAnd
            | BoolOr
            | BoolXor
            deriving (Eq, Ord, Read, Show)

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
