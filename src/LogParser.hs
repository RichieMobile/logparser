module LogParser where

import System.IO
import Data.List

type LogFilePath = String
type ConfigFilePath = String
type Matcher = String

parse :: LogFilePath -> ConfigFilePath -> IO String
parse l c = do 
    logContents <- readFile l
    let line = lines logContents
    return logContents

parseWithMatcher :: Matcher -> [String] -> String
parseWithMatcher m xs = m ++ " = " ++ (show $ countLines m xs)

countLines :: Matcher -> [String] -> Int
countLines _ [] = 0 
countLines "" _ = 0
countLines m (x:xs) = (count m x) + (countLines m xs)
-- non-recursive verison, not sure how haskell handles tail optimization
-- countLines m xs = foldl (\a b -> (count m b) + a) 0 xs

count :: Matcher -> String -> Int 
count m s = if isInfixOf m s then 1 else 0