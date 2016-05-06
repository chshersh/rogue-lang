module Main where

import System.Environment (getArgs)
import Rogue.Compiler

main :: IO ()
main = do
    [fileName] <- getArgs
    compileAndRun fileName