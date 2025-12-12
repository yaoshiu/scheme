{-# LANGUAGE OverloadedStrings #-}
module Parser where

import LispVal
import Data.Void
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
