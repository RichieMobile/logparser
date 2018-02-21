module Main where

import System.Environment
import qualified LogParser as L

main :: IO ()
main = do
    (a:as) <- getArgs
    let config = a
    let logs = as
    putStrLn $ "Config: " ++ config
    putStrLn $ "Log: " ++ (show logs)
    putStrLn "\n--------"
    putStrLn "Analyzing Log"
    putStrLn "--------\n"

    output <- L.parse config logs
    putStrLn output
