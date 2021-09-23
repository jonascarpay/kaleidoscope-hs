{-# LANGUAGE BangPatterns #-}

module Token
  ( Token (..),
    SourcePos (..),
    tok,
    tok_frac,
    tok_int,
    tok_ident,
    descrToken,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

data SourcePos = SourcePos
  { lineNumber :: {-# UNPACK #-} !Int,
    columnNumber :: {-# UNPACK #-} !Int,
    byteOffset :: {-# UNPACK #-} !Int
  }
  deriving (Eq)

instance Show SourcePos where
  show (SourcePos l c _) = "L" <> show l <> ":" <> show c

data Token
  = EOF
  | If
  | Then
  | Else
  | Def
  | Extern
  | Ident Text
  | Num Double
  | Add
  | Sub
  | Mul
  | Div
  | Lt
  | Gt
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semicolon
  | Comma
  deriving (Eq, Show)

tok :: Token -> a -> Token
tok = const

{-# ANN module ("hlint: ignore Use camelCase" :: String) #-}

unsafeParseInt :: ByteString -> Int
unsafeParseInt = BS.foldl' f 0
  where
    f acc b = acc * 10 + fromIntegral b - 48

unsafeParseFrac :: ByteString -> Double
unsafeParseFrac = fst . BS.foldl' f (0, 0.1)
  where
    f (!acc, !fac) b = (acc + fac * fromIntegral (b -48), fac * 0.01)

tok_int :: ByteString -> Token
tok_int = Num . fromIntegral . unsafeParseInt

tok_frac :: ByteString -> Token
tok_frac bs = Num $ case BS.split 46 bs of
  [int, frac] -> fromIntegral (unsafeParseInt int) + unsafeParseFrac frac
  _ -> error "impossible parse for a number"

tok_ident :: ByteString -> Token
tok_ident = Ident . TE.decodeUtf8

descrToken :: Token -> String
descrToken (Ident x) = "identifier " <> show x
descrToken Add = "+"
descrToken Mul = "*"
descrToken Div = "/"
descrToken Sub = "-"
descrToken Gt = ">"
descrToken Lt = "<"
descrToken LParen = "("
descrToken RParen = ")"
descrToken LBrace = "{"
descrToken RBrace = "}"
descrToken x = toLower <$> show x
