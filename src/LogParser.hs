module LogParser where

import System.IO
import Data.List
import Data.Aeson
import GHC.Generics
import Domain.ConfigRules
import Domain.Rule
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Matchers.Parser as P

type LogFilePath = String
type ConfigFilePath = String

parse :: ConfigFilePath -> [LogFilePath] -> IO String
parse c l = do 
    configContents <- B.readFile c
    logContents <- readLogFiles l
    let parsedRules = rules $ decodeConfig configContents
    let output = foldl (\o m -> o ++ (P.parse m logContents) ++ "\n") "" parsedRules
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