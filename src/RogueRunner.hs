module RogueRunner where

import System.FilePath (takeBaseName)

import RogueTokens
import RogueParser     (parseRogue)
import RogueEmitter    (codegenLLVM)
import RogueJit        (runJIT)

compileAndRun :: FilePath -> IO ()
compileAndRun fileName = do
    fileContent <- readFile fileName

    let parseResult = parseRogue fileContent 1

    case parseResult of
         Ok ast -> do
            let moduleName      = takeBaseName fileName
            let codegenedModule = codegenLLVM moduleName ast

            runJIT codegenedModule
            return ()
            
         Failed errMsg -> putStrLn errMsg      
    
    