module RogueRunner where

import System.FilePath (takeBaseName)

import RogueLexer      (lexer)
import RogueParser     (parseRogue)
import RogueEmitter    (codegenLLVM)
import RogueJit        (runJIT)

compileAndRun :: FilePath -> IO ()
compileAndRun fileName = do
    fileContent <- readFile fileName

    let tokens          = lexer fileContent
    let ast             = parseRogue tokens
    let moduleName      = takeBaseName fileName
    let codegenedModule = codegenLLVM moduleName ast

    runJIT codegenedModule
    
    return ()