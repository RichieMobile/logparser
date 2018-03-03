module Matchers.Parser where

class Prs a where
    parse :: a -> [FilePath] -> IO String