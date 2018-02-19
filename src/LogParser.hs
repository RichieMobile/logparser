{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LogParser where

import System.IO
import Data.List
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

type LogFilePath = String
type ConfigFilePath = String
type Matcher = String

data Rule = Rule {
    matcher :: Matcher
} deriving (Eq, Generic, Show)

data ConfigRules = ConfigRules {
    rules :: [Rule]
} deriving (Eq, Generic, Show)

emptyConfig :: ConfigRules
emptyConfig = ConfigRules [Rule ""]

instance FromJSON Rule
instance FromJSON ConfigRules

parse :: LogFilePath -> ConfigFilePath -> IO String
parse l c = do 
    configContents <- B.readFile c
    logContents <- readFile l
    let parsedRules = case (decode configContents) of
                        Nothing -> rules emptyConfig
                        Just value -> rules value
    let line = lines logContents
    let output = foldl (\o m -> o ++ (parseWithMatcher (matcher m) line) ++ "\n") "" parsedRules
    return output

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