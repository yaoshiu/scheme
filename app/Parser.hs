{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import LispVal (LispVal (Atom, Bool, List, Nil, Number, String))
import Text.Megaparsec
  ( MonadParsec (eof, try),
    ParseErrorBundle,
    Parsec,
    between,
    many,
    manyTill,
    oneOf,
    parse,
    some,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
  )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type Error = ParseErrorBundle Text Void

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identStart :: Parser Char
identStart = letterChar <|> oneOf ("-+/*=|&><" :: String)

identLetter :: Parser Char
identLetter = alphaNumChar <|> oneOf ("?+=|&-/" :: String)

identifier :: Parser Text
identifier = lexeme $ do
  x <- identStart
  xs <- many identLetter
  pure $ T.pack $ x : xs

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

opLetter :: Parser Char
opLetter = oneOf (":!#$%&*+./<=>?@\\^|-~" :: String)

operator :: Parser Text
operator = lexeme $ T.pack <$> some opLetter

parseAtom :: Parser LispVal
parseAtom = Atom <$> (identifier <|> operator)

parseText :: Parser LispVal
parseText = String . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

parseNumber :: Parser LispVal
parseNumber = Number <$> L.signed sc L.decimal

parseList :: Parser LispVal
parseList = List <$> many parseExpr

parseSExp :: Parser LispVal
parseSExp = List <$> parens (many parseExpr)

reserved :: Text -> LispVal -> Parser LispVal
reserved s v = symbol s *> pure v

parseQuote :: Parser LispVal
parseQuote = do
  _ <- char '\''
  x <- parseExpr
  pure $ List [Atom "quote", x]

parseReserved :: Parser LispVal
parseReserved =
  reserved "Nil" Nil
    <|> reserved "#t" (Bool True)
    <|> reserved "#f" (Bool False)

parseExpr :: Parser LispVal
parseExpr =
  parseReserved
    <|> try parseNumber
    <|> parseAtom
    <|> parseText
    <|> parseQuote
    <|> parseSExp

contents :: Parser a -> Parser a
contents = between sc eof

readExpr :: Text -> Either Error LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: Text -> Either Error LispVal
readExprFile = parse (contents parseList) "<file>"
