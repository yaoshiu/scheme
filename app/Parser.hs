{-# LANGUAGE OverloadedStrings #-}

module Parser (expr, program, single) where

import Control.Applicative (many)
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, manyTill, takeWhile1P, try, (<|>), MonadParsec (..))
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import SExpr (SExpr (..), quote)

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

number :: Parser SExpr
number = SNum <$> lexeme (L.signed (pure ()) L.decimal)

symbolP :: Parser SExpr
symbolP = lexeme . try $ do
  s <- symbolName
  if s == "."
    then fail "`.` cannot be a symbol"
    else pure (SSym s)

symbolName :: Parser Text
symbolName = takeWhile1P (Just "symbol") isSymbolChar

isSymbolChar :: Char -> Bool
isSymbolChar c =
  isAlphaNum c
    || not (c `elem` ("'\",[]{}()#`;," :: String)) && not (isSpace c)

quoted :: Parser SExpr
quoted = do
  _ <- lexeme (char '\'')
  e <- expr
  pure $ SPair quote $ SPair e SNil

characterP :: Parser SExpr
characterP = lexeme $ do
  _ <- char '#'
  c <- specialChar <|> L.charLiteral
  pure (SChar c)
  where
    specialChar :: Parser Char
    specialChar = try (string "\\w" >> pure ' ')

stringP :: Parser SExpr
stringP = lexeme $ do
  _ <- char '"'
  chars <- manyTill L.charLiteral (char '"')
  let str = foldr (SPair . SChar) SNil chars 
  pure $ SPair quote $ SPair str SNil

atom :: Parser SExpr
atom = try number <|> characterP <|> stringP <|> quoted <|> symbolP

expr :: Parser SExpr
expr = atom <|> list

list :: Parser SExpr
list = between (symbol "(") (symbol ")") pairRest

pairRest :: Parser SExpr
pairRest = (SNil <$ lookAhead (symbol ")"))
  <|> do
    h <- expr
    (do
        _ <- symbol "."
        t <- expr
        pure $ SPair h t
      ) <|> (SPair h <$> pairRest)

program :: Parser [SExpr]
program = sc *> many expr <* eof

single :: Parser SExpr
single = sc *> expr <* eof
