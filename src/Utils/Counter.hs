module Utils.Counter where

import Data.List

countLines :: String -> [String] -> Int
countLines _ [] = 0 
countLines "" _ = 0
countLines m (x:xs) = (count m x) + (countLines m xs)

count :: String -> String -> Int 
count m s = if isInfixOf m s then 1 else 0