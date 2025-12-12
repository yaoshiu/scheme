{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import Data.Void
import LispVal
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

quoted :: Parser a -> Parser a
quoted = (*>) $ char '\''

identStart :: Parser Char
identStart = letterChar <|> oneOf ("!$%&*/:<=>?^_~" :: String)

identLetter :: Parser Char
identLetter = alphaNumChar <|> oneOf ("!$%&*/:<=>?^_~" :: String)

identifier :: Parser Text
identifier = lexeme $ T.pack <$> ((:) <$> identStart <*> many identifier <|> specialIdentifier) <?> "identifier"

specialIdentifier :: Parser String
specialIdentifier =
  try $
    string "-"
      <|> string "+"
      <|> string "..."
