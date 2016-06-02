{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Test.Rogue.Runner.ProcessLLVM
    ( createProcessLLVM
    ) where

import Prelude hiding (putStrLn)

import Data.Monoid    ((<>))

import Data.Text.IO   (hGetLine, hPutStrLn, putStrLn)
import Data.Text.Read (decimal)

import System.Process (StdStream (CreatePipe), createProcess,
                       proc, std_in, std_out)

createProcessLLVM :: IO Int
createProcessLLVM = do
    let runningProcess  = proc "stack" [ "exec"
                                       , "--", "roguec"
                                       , "--file", "../rg-test/test.rg"
                                       ]
    let hijackedProcess = runningProcess { std_in = CreatePipe, std_out = CreatePipe }

    (Just hin, Just hout, _, _) <- createProcess hijackedProcess

    hPutStrLn hin "5"
    hPutStrLn hin "9"

    parseResult <- decimal <$> hGetLine hout
    return $ either error fst parseResult