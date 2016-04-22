module Main where

import System.Environment (getArgs)

import Rogue.Compiler     (compileAndRun)

main :: IO ()
main = do
    [fileName] <- getArgs
    compileAndRun fileName

