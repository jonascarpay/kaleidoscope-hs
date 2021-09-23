module AST where

import Data.Text (Text)

type Ident = Text

data TopLevel
  = TLExt Ext
  | TLDef Def
  | TLExpr Expr

data Expr
  = Var Ident
  | Bin Op Expr Expr
  | Call Ident [Expr]

data Def = Def Ident [Ident] Expr

data Ext = Ext

data Op = Add | Sub | Mul | Div