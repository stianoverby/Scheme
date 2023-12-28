module Main (main) where

import System.Environment
import Lib(safeHead)
import Parser(parseProgram)

main :: IO ()
main =  do  args <- getArgs
            let first = case safeHead args of
                        Nothing -> ""
                        Just input  -> input
            putStrLn $ case parseProgram first of
                Left  err       -> "Parse error: " ++ show err
                Right ast       -> show ast
