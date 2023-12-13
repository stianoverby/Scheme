module Syntax(
    LispVal (..)
) where

data LispVal
    = Atom      String
    | List      [LispVal]
    | DotList   [LispVal] LispVal
    | Number    Integer
    | LispStr   String
    | Boolean   Bool
    deriving (Show, Eq)