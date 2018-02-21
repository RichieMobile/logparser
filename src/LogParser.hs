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
import qualified Data.ByteString.Char8 as C

type LogFilePath = String
type ConfigFilePath = String

parse :: ConfigFilePath -> [LogFilePath] -> IO String
parse c l = do 
    configContents <- B.readFile c
    logContents <- readFiles l
    let parsedRules = rules $ decodeConfig configContents
    let output = foldl (\o m -> o ++ (parseWithRule m logContents) ++ "\n") "" parsedRules
    return output

readFiles :: [FilePath] -> IO [String]
-- readFiles = fmap concat . lines . readFile
readFiles files = do
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
        (format ((show mCount) ++ "/" ++ (show $ mCount + cCount)) roundedRatio)


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