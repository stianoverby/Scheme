module Lib(safeHead) where

safeHead :: [a] -> Maybe a
safeHead []        = Nothing
safeHead (x:xs)    = Just x
