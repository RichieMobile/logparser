module LogParser where

import System.IO
import Data.List
import Data.Aeson
import GHC.Generics
import Domain.ConfigRules
import Domain.Rule
import Numeric
import qualified Data.ByteString.Lazy as B

type LogFilePath = String
type ConfigFilePath = String

parse :: LogFilePath -> ConfigFilePath -> IO String
parse l c = do 
    configContents <- B.readFile c
    logContents <- readFile l
    let parsedRules = rules $ decodeConfig configContents
    let line = lines logContents
    let output = foldl (\o m -> o ++ (parseWithRule m line) ++ "\n") "" parsedRules
    return output

parseWithRule :: Rule -> [String] -> String
parseWithRule r xs = case (processor r) of
        "count" -> parseWithMatcher (matcher r) xs
        "ratio" -> parseWithRatio (matcher r) (comparitor r) xs 
        _ -> "Unknown processor!"

parseWithMatcher :: Matcher -> [String] -> String
parseWithMatcher m xs = m ++ " = " ++ (show $ countLines m xs)

parseWithRatio :: Matcher -> String -> [String] -> String
parseWithRatio m c xs = 
    let (mCount, cCount, ratio) = calculateRatio m c xs
        roundedRatio = showFFloat (Just 2) ratio ""
        format f s = f ++ " = " ++ (show s)
    in  (format m mCount) ++ "\n" ++ 
        (format c cCount) ++ "\n" ++ 
        (format (m ++ "/" ++ c) roundedRatio)


calculateRatio :: Matcher -> String -> [String] -> (Float, Float, Float)
calculateRatio m c xs = 
    let mCount = fromIntegral $ countLines m xs 
        cCount = fromIntegral $ countLines c xs
        ratio = if cCount == 0 then 1.0
                else (mCount / cCount)
    in (mCount, cCount, ratio)

countLines :: Matcher -> [String] -> Int
countLines _ [] = 0 
countLines "" _ = 0
countLines m (x:xs) = (count m x) + (countLines m xs)

count :: Matcher -> String -> Int 
count m s = if isInfixOf m s then 1 else 0