module LogParser where

import System.IO
import Data.List
import Data.Aeson
import GHC.Generics
import Domain.ConfigRules
import Domain.Rule
import Numeric
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Matchers.TimeAverager as T 

type LogFilePath = String
type ConfigFilePath = String

parse :: ConfigFilePath -> [LogFilePath] -> IO String
parse c l = do 
    configContents <- B.readFile c
    logContents <- readLogFiles l
    let parsedRules = rules $ decodeConfig configContents
    let output = foldl (\o m -> o ++ (parseWithRule m logContents) ++ "\n") "" parsedRules
    return output

readLogFiles :: [FilePath] -> IO [String]
readLogFiles files = do
    fileLines <- foldM readLogFile [] files
    return fileLines

readLogFile :: [String] -> FilePath -> IO [String]
readLogFile a f = do 
    file <- readFile f
    let fileLines = lines file
    return $ concat (fileLines:[a])

parseWithRule :: Rule -> [String] -> String
parseWithRule r xs = case (processor r) of
        "count" -> "\nCount Match\n" ++ parseWithMatcher (matcher r) xs
        "ratio" -> "\nRatio Match\n" ++ parseWithRatio (matcher r) (comparitor r) xs 
        "avgTime" -> T.parseAverageTime (matcher r) (comparitor r) (timestampFormat r) (range r) xs
        _ -> "Unknown processor!"

parseWithMatcher :: Matcher -> [String] -> String
parseWithMatcher m xs = m ++ " = " ++ (show $ countLines m xs)

parseWithRatio :: Matcher -> String -> [String] -> String
parseWithRatio m c xs = 
    let (mCount, cCount, ratio) = calculateRatio m c xs
        roundedRatio = showFFloat (Just 2) (ratio * 100) "%"
        roundedRatioFailed = showFFloat (Just 2) ((1.0 - ratio) * 100) "%"
        format f s = f ++ " = " ++ (show s)
    in  ("Successful: " ++ (format m mCount)) ++ "\n" ++ 
        ("Failed: " ++ (format c cCount)) ++ "\n" ++ 
        ("Successful %: " ++ roundedRatio) ++ "\n" ++
        ("Failed %: " ++ roundedRatioFailed) ++ "\n"

calculateRatio :: Matcher -> String -> [String] -> (Float, Float, Float)
calculateRatio m c xs = 
    let mCount = fromIntegral $ countLines m xs 
        cCount = fromIntegral $ countLines c xs
        ratio = if cCount == 0 then 1.0
                else (mCount / (cCount + mCount))
    in (mCount, cCount, ratio)

countLines :: Matcher -> [String] -> Int
countLines _ [] = 0 
countLines "" _ = 0
countLines m (x:xs) = (count m x) + (countLines m xs)

count :: Matcher -> String -> Int 
count m s = if isInfixOf m s then 1 else 0