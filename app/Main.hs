module Main where

import System.Environment
import qualified LogParser as L

main :: IO ()
main = do
    args <- getArgs
    let logFile = args !! 0
    let config = args !! 1
    putStrLn $ "Log: " ++ logFile
    putStrLn $ "Config: " ++ config
    putStrLn "\n--------"
    putStrLn "Analyzing Log"
    putStrLn "--------\n"

    output <- L.parse logFile config
    putStrLn output
