module Parser where

import Text.Parsec
  ( ParseError,
    alphaNum,
    char,
    digit,
    many1,
    newline,
    parse,
    space,
    string,
    (<|>),
  )
import Text.Parsec.String (Parser)

idP :: Parser String
idP = many1 (alphaNum <|> char '_')

intP :: Parser Int
intP = read <$> ((:) <$> char '-' <*> many1 digit <|> many1 digit)

commaSpaceP :: Parser String
commaSpaceP = string ", "

betweenChars :: Char -> Parser a -> Char -> Parser a
betweenChars l p r = char l *> p <* char r

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

testFileParse :: Parser a -> Char -> Parser (Int, a)
testFileParse p c = (,) <$> (string [c, c] *> space *> string "Expected:" *> space *> intP) <* newline <*> p
