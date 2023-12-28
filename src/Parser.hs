module Parser(parseProgram)
where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (void)
import Data.Char (isAscii, digitToInt)
import Data.List (find)

import Syntax (LispVal (..))

parseProgram :: String -> Either ParseError LispVal
parseProgram = parse (whitespace *> program <* eof) "lisp"

whitespace :: Parser ()
whitespace = void $ many space

program :: Parser LispVal
program = expr

expr :: Parser LispVal
expr =  numberLit
    <|> stringLit
    <|> atom

atom :: Parser LispVal
atom = do   first <- letter <|> symbol
            rest  <- many (letter <|> digit <|> symbol)
            let a = [first] ++ rest
            return $ case a of
                "#t"        -> Boolean True
                "#f"        -> Boolean False
                otherwise   -> Atom a

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

quotes :: Parser a -> Parser a
quotes = between (char '"') (char '"')

stringLit :: Parser LispVal
stringLit = LispStr <$> (quotes $ many (ordinary <|> escaped))
    where   ordinary = space <|> satisfy (\c -> isAscii c && c `notElem` "\\\"")
            escaped = char '\\' >>
                choice 
                    [
                        char '"'
                    ,   char '\\'
                    ,   char 'n' >> return '\n'
                    ,   char '\n'
                    ,   char '\r'
                    ]

numberLit :: Parser LispVal
numberLit = Number <$> 
    choice
        [ 
            prefixedNumber
        ,   decimal
        ]

prefixedNumber :: Parser Integer
prefixedNumber = char '#' >> 
    choice 
        [
            char 'd' >> decimal
        ,   char 'b' >> binary
        ,   char 'h' >> hex
        ,   char 'o' >> octal
        ] 

decimal :: Parser Integer
decimal = read           <$> many1 digit

binary :: Parser Integer
binary  = binstr2Integer <$> many1 (oneOf binDigits)

hex :: Parser Integer
hex     = hexstr2Integer <$> many1 (oneOf hexDigits)

octal :: Parser Integer
octal   = octstr2Integer <$> many1 (oneOf octDigits)

binstr2Integer :: String -> Integer
binstr2Integer = foldl (\acc b -> acc * 2  + (toInteger . digitToInt) b) 0

hexstr2Integer :: String -> Integer
hexstr2Integer = foldl (\acc h -> acc * 16 + (toInteger . hexToInt  ) h) 0

octstr2Integer :: String -> Integer
octstr2Integer = foldl (\acc o -> acc * 8  + (toInteger . digitToInt) o) 0

decDigits, binDigits, hexDigits, octDigits :: [Char]
decDigits = "0123456789"
binDigits = "01"
hexDigits = "0123456789ABCDEF"
octDigits = "01234567"

-- Assume character is an element of the set of hex digits. Undefined for
-- for other characters.
hexToInt :: Char -> Int
hexToInt hex = case find (\(hex', d) -> hex == hex') valueMap of
    Nothing     -> undefined
    Just (k,v)  -> v 
    where valueMap = zip hexDigits [0 .. ]
