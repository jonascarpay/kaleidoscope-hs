module Parser where

import AST
import Control.Applicative.Combinators
import Data.Set (Set)
import Data.Set qualified as S
import Parsec qualified as P
import Token (Token)

type Parser = P.Parser Token (Set String)

pTop :: Parser TopLevel
pTop =
  choice
    [ TLExt <$> pExt,
      TLDef <$> pDef,
      TLExpr <$> pExpr
    ]

pExt :: Parser Ext
pExt = undefined

pDef :: Parser Def
pDef = undefined

pExpr :: Parser Expr
pExpr =
  choice
    []

expect :: String -> (Token -> Maybe a) -> Parser a
expect str k = P.throwAt $ \throw ->
  P.token >>= maybe (throw (S.singleton str)) pure . k
