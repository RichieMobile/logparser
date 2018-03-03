{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Matchers.Counter where

import Data.List
import Data.Aeson
import GHC.Generics
import Matchers.Parser
import Utils.Counter

data Counter = Counter {
      matcher :: String
} deriving (Eq, Generic, Show)

instance FromJSON Counter
instance Prs Counter where
    parse a paths = do
        counts <- mapM (\f -> parseFiles a f) paths
        let s = sum counts
        let output = (matcher a) ++ " = " ++ (show s)
        return output

parseFiles :: Counter -> FilePath -> IO Int
parseFiles a path = do
    contents <- readFile path
    let l = lines contents
    return $ countLines (matcher a) l

parseWithMatcher :: String -> [String] -> String
parseWithMatcher m xs = m ++ " = " ++ (show $ countLines m xs)