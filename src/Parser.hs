{-# LANGUAGE LambdaCase #-}

module Parser where

import AST
import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad.Combinators.Expr
import Data.Set (Set)
import Data.Set qualified as S
import Parsec qualified as P
import Token (Token)
import Token qualified as T

type Parser = P.Parser Token (Set String)

pTop :: Parser TopLevel
pTop =
  choice
    [ TLExt <$> pExt,
      TLDef <$> pDef,
      TLExpr <$> pExpr
    ]

pExt :: Parser Ext
pExt = liftA2 Ext pIdent (parenList pIdent)

pDef :: Parser Def
pDef = liftA3 Def pIdent (parenList pIdent) pExpr

pExpr :: Parser Expr
pExpr =
  makeExprParser
    pTerm
    [ [bin T.Lt Lt],
      [bin T.Mul Mul, bin T.Div Div],
      [bin T.Add Add, bin T.Add Add]
    ]
  where
    bin :: Token -> Op -> Operator Parser Expr
    bin tk op = InfixL $ Bin op <$ token tk

pTerm :: Parser Expr
pTerm =
  choice
    [ Num <$> pNum,
      liftA2 Call pIdent (parenList pExpr),
      Var <$> pIdent
    ]

pIdent :: Parser Ident
pIdent = expect "identifier" $ \case
  T.Ident txt -> pure txt
  _ -> Nothing

parenList :: Parser a -> Parser [a]
parenList p = parens $ sepBy p (token T.Comma)

parens :: Parser a -> Parser a
parens p = token T.LParen *> p <* token T.RParen

pNum :: Parser Double
pNum = expect "number" $ \case
  T.Num x -> Just x
  _ -> Nothing

token :: Token -> Parser ()
token tk = P.throwAt $ \throw ->
  P.token >>= \tk' ->
    if tk == tk'
      then pure ()
      else throw (S.singleton (T.descrToken tk))

expect :: String -> (Token -> Maybe a) -> Parser a
expect str k = P.throwAt $ \throw ->
  P.token >>= maybe (throw (S.singleton str)) pure . k
