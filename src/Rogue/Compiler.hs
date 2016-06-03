module Rogue.Compiler
    ( compileFile
    , moduleToString
    , runModule
    ) where

import           System.FilePath          (takeBaseName)

import           Control.Monad.Except     (runExcept)
import           Control.Monad.State      (evalStateT)
import           Data.Functor             (void)

import           LLVM.General.AST         (Module)

import           Rogue.LLVM.Emitter       (codegenLLVM)
import           Rogue.LLVM.JIT           (moduleToString, runJIT)
import           Rogue.Parser.ParserMonad (ParserM (unParserM), ParserState (..))
import           Rogue.Parser.SuperParser (parseRogue)
import           Rogue.Parser.Tokens      (Identifier)
import           Rogue.Verify.Verifier    (verify)

compileFile :: FilePath -> IO Module
compileFile inputFileName = do
    fileContent   <- readFile inputFileName
    let moduleName = takeBaseName inputFileName
    compileModule moduleName fileContent

compileModule :: Identifier -> String -> IO Module
compileModule moduleName moduleContent = do
    let fixedContent = moduleContent ++ "\n"
    let parserState  = ParserState { _fileName = moduleName
                                   , _inputStream = fixedContent
                                   , _lineNumber = 1
                                   , _column = 0
                                   }
    let parseResult  = evalStateT (unParserM parseRogue) parserState

    case runExcept parseResult of
         Left errMsg -> error errMsg  -- TODO: handle errors properly
         Right ast   -> do
            let verifiedAst = verify ast
            return $ codegenLLVM moduleName verifiedAst

runModule :: Module -> IO ()
runModule compiledModule = void $ runJIT compiledModule