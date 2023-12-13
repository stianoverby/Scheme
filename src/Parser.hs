module Parser(parseProgram)
where

import Syntax (LispVal (..))
import Text.ParserCombinators.Parsec hiding (spaces)

parseProgram :: String -> String
parseProgram input = case parse expr "lisp" input of
    Left error -> "No match: "      ++ show error
    Right val  -> "Found value: "   ++ show val

spaces :: Parser ()
spaces = skipMany space 

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

expr :: Parser LispVal
expr =  atom
    <|> str
    <|> number

str :: Parser LispVal
str =  LispStr <$> quotes text

quotes :: Parser a -> Parser a
quotes =  between (char '"') (char '"')

text :: Parser String
text = many $ noneOf "\""

atom :: Parser LispVal
atom = do   first <- letter <|> symbol
            rest  <- many (letter <|> digit <|> symbol)
            let atom = [first] ++ rest
            return $ case atom of
                "#t"        -> Boolean True
                "#f"        -> Boolean False
                otherwise   -> Atom atom

number :: Parser LispVal
number = (Number . read) <$> (many1 digit)