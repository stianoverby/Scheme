module Lib(mHead) where

mHead :: [a] -> Maybe a
mHead []        = Nothing
mHead (x:xs)    = Just x
