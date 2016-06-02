module Rogue.Compiler
    ( compileFile
    , moduleToString
    , runModule
    ) where

import System.FilePath          (takeBaseName)

import Data.Functor             (void)
import Control.Monad.Except     (runExcept)
import Control.Monad.State      (evalStateT)

import LLVM.General.AST         (Module)

import Rogue.Parser.ParserMonad (ParserM (unParserM), ParserState (..))
import Rogue.Parser.Tokens      (Identifier)
import Rogue.Parser.SuperParser (parseRogue)
import Rogue.Verify.Verifier    (verify)
import Rogue.LLVM.Emitter       (codegenLLVM)
import Rogue.LLVM.JIT           (runJIT, moduleToString)

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