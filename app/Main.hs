module Main where

import RogueLexer
import RogueParser
import RogueCompiler
import RogueEmitter

main :: IO ()
main = do
    testFile <- readFile "./test/testProgram.rg"

    let tokens = lexer testFile
    print tokens
    putStrLn "-------------------------------------------------------"

    let ast = parseRogue tokens
    print ast
    putStrLn "-------------------------------------------------------"

    -- let checkedAST = transformFunCallsToVars ast
    -- print checkedAST
    -- putStrLn "-------------------------------------------------------"

    codegenLLVM initModule ast
    return ()

