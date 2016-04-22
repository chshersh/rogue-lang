module Rogue.Compiler where

import System.FilePath          (takeBaseName)

import Control.Monad.Except
import Control.Monad.State

import Rogue.Parser.ParserMonad
import Rogue.Parser.SuperParser (parseRogue)
import Rogue.LLVM.Emitter       (codegenLLVM)
import Rogue.LLVM.JIT           (runJIT)

compileAndRun :: FilePath -> IO ()
compileAndRun inputFileName = do
    fileContent <- readFile inputFileName

    let moduleName  = takeBaseName inputFileName
    let parseResult = evalStateT (unParserM parseRogue) $ ParserState { _fileName = moduleName, _inputStream = fileContent, _lineNumber = 1, _column = 0 }

    case runExcept parseResult of
         Left errMsg -> putStrLn errMsg
         Right ast   -> do
            let codegenedModule = codegenLLVM moduleName ast
            runJIT codegenedModule
            return ()