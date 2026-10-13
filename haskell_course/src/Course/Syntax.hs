module Course.Syntax where

import Data.Int (Int64)
import Text.Parsec.Pos (SourcePos)

type Name = String

data Language = Caml | Lisp deriving (Eq, Show)
data Target = AArch64Linux | X86_64Linux deriving (Eq, Show)

data Type = TInt | TBool | TUnit | TFunction [Type] Type deriving (Eq, Show)

data Op = Add | Sub | Mul | Div | Eq | Ne | Lt | Gt | Le | Ge
        | And | Or | Xor | Sequence deriving (Eq, Show)

data Expr = Expr { exprPosition :: SourcePos, exprNode :: ExprNode }
  deriving (Eq, Show)

data ExprNode
  = Number Int64
  | Boolean Bool
  | Unit
  | Apply Name [Expr]
  | Binary Op Expr Expr
  | Let Name Expr Expr
  | If Expr Expr Expr
  deriving (Eq, Show)

data Function = Function
  { functionPosition :: SourcePos
  , functionName :: Name
  , functionParameters :: [(Name, Type)]
  , functionResult :: Type
  , functionBody :: Expr
  } deriving (Eq, Show)

type Program = [Function]

operatorNames :: [(String, Op)]
operatorNames =
  [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("==", Eq), ("!=", Ne)
  ,("<", Lt), (">", Gt), ("<=", Le), (">=", Ge), ("&&", And), ("and", And)
  ,("||", Or), ("or", Or), ("^", Xor), ("xor", Xor), ("begin", Sequence)]

prettyType :: Type -> String
prettyType TInt = "int"
prettyType TBool = "bool"
prettyType TUnit = "()"
prettyType (TFunction args result) = "[" ++ unwords (map prettyType args) ++ "] " ++ prettyType result
