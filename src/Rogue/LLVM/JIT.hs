module Rogue.LLVM.JIT where

import           Data.Int
import           Data.Word
import           Foreign.Ptr ( FunPtr, castFunPtr )

import           Control.Monad.Except

import           LLVM.General.Target
import           LLVM.General.Context
import           LLVM.General.CodeModel
import           LLVM.General.Module          as Mod
import qualified LLVM.General.AST             as AST

import           LLVM.General.PassManager
import           LLVM.General.Transforms
import           LLVM.General.Analysis

import qualified LLVM.General.ExecutionEngine as EE

foreign import ccall "dynamic"
    llvmToHaskellFunction :: FunPtr (IO ()) -> (IO ())

runForeign :: FunPtr a -> IO ()
runForeign fun = llvmToHaskellFunction $ castFunPtr fun

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT mod = do
  withContext $ \context ->
      jit context $ \executionEngine ->
          runExceptT $ withModuleFromAST context mod $ \m ->  -- TODO: handle errors properly
              withPassManager passes $ \pm -> do
                  -- Optimization Pass
                  {-runPassManager pm m-}
                  optimizedModule <- moduleAST m

                  EE.withModuleInEngine executionEngine m $ \ee -> do
                      mainfn <- EE.getFunction ee (AST.Name "main")
                      case mainfn of
                          Just fn -> runForeign fn
                          Nothing -> return ()

                  -- Return the optimized module
                  return optimizedModule

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

stringLLVMRepresentation :: AST.Module -> IO String
stringLLVMRepresentation m = withContext $ \context ->
    liftError $ withModuleFromAST context m moduleLLVMAssembly