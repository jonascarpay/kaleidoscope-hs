module AST where

import Data.Text (Text)

type Ident = Text

data TopLevel
  = TLProto FnProto
  | TLDef FnDef
  | TLExpr Expr

data Expr
  = Var Ident
  | Num Double
  | Bin Op Expr Expr
  | Call Ident [Expr]

data FnDef = FnDef FnProto Expr

data FnProto = FnProto Ident [Ident]

data Op = Add | Sub | Mul | Div | Lt
