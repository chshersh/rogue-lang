module Main where

import System.Environment (getArgs)

import RogueRunner        (compileAndRun)

main :: IO ()
main = do
    [fileName] <- getArgs
    compileAndRun fileName

