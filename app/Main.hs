module Main where

import RogueLexer
import RogueParser
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

    codegenLLVM initModule ast
    return ()

