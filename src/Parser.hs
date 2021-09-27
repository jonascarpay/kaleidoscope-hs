{-# LANGUAGE LambdaCase #-}

module Parser (parse) where

import AST
import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad.Combinators.Expr
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector qualified as V
import Lexer (lexer)
import Parsec qualified as P
import Token (Token)
import Token qualified as T

type Parser = P.Parser Token (Set String)

parse :: ByteString -> Either String TopLevel
parse bs = do
  case lexer bs of
    Left pos -> Left (show pos)
    Right ts ->
      let v = V.fromList ((fst <$> ts) <> [T.EOF])
       in first show $ P.runParser (v V.!) (pTop <* token T.EOF)

pTop :: Parser TopLevel
pTop =
  choice
    [ TLProto <$> pProto,
      TLDef <$> pDef,
      TLExpr <$> pExpr
    ]

pProto :: Parser FnProto
pProto = token T.Extern >> liftA2 FnProto pIdent (parenList pIdent)

pDef :: Parser FnDef
pDef = token T.Def >> liftA3 (\fn args body -> FnDef (FnProto fn args) body) pIdent (parenList pIdent) pExpr

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
