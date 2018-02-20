module LogParser where

import System.IO
import Data.List
import Data.Aeson
import GHC.Generics
import Domain.ConfigRules
import Domain.Rule
import qualified Data.ByteString.Lazy as B

type LogFilePath = String
type ConfigFilePath = String

parse :: LogFilePath -> ConfigFilePath -> IO String
parse l c = do 
    configContents <- B.readFile c
    logContents <- readFile l
    let parsedRules = rules $ decodeConfig configContents
    let line = lines logContents
    let output = foldl (\o m -> o ++ (parseWithMatcher (matcher m) line) ++ "\n") "" parsedRules
    return output

parseWithMatcher :: Matcher -> [String] -> String
parseWithMatcher m xs = m ++ " = " ++ (show $ countLines m xs)

countLines :: Matcher -> [String] -> Int
countLines _ [] = 0 
countLines "" _ = 0
countLines m (x:xs) = (count m x) + (countLines m xs)

count :: Matcher -> String -> Int 
count m s = if isInfixOf m s then 1 else 0