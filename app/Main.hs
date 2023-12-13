module Main (main) where

import System.Environment
import Lib(mHead)
import Parser(parseProgram)

main :: IO ()
main =  do  args <- getArgs
            let first = case mHead args of
                    Nothing -> ""
                    Just s  -> s
            putStrLn $ parseProgram first
