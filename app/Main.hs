module Main where

import System.Environment (getArgs)

import Rogue.Runner       (compileAndRun)

main :: IO ()
main = do
    [fileName] <- getArgs
    compileAndRun fileName

