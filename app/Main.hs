{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad      (when, (>=>))

import           GHC.Generics       (Generic)

import           Options.Generic    (ParseRecord, getRecord, unHelpful, (<?>)(..))

import           Rogue.Compiler     (compileFile, moduleToString, runModule)

data CompilerOptions = CompilerOptions
    { file    :: FilePath <?> "Path to file with Rogue code"
    , verbose :: Bool     <?> "Print compiled llvm code in stdout"
    } deriving (Generic)

instance ParseRecord CompilerOptions

main :: IO ()
main = do
    CompilerOptions{..} <- getRecord "Rogue compiler, version: 0.1"
    let filePath  = unHelpful file
    let isVerbose = unHelpful verbose

    compiledModule <- compileFile filePath
    when isVerbose $ moduleToString >=> putStrLn $ compiledModule
    runModule compiledModule