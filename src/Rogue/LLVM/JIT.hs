module Rogue.LLVM.JIT
    ( runJIT
    , moduleToString
    ) where

import           Foreign.Ptr                  (FunPtr, castFunPtr)

import           Control.Monad                ((>=>))
import           Control.Monad.Except         (ExceptT, runExceptT)

import qualified LLVM.General.AST             as AST
import           LLVM.General.Context         (Context, withContext)
import qualified LLVM.General.ExecutionEngine as EE
import           LLVM.General.Module          (moduleAST, moduleLLVMAssembly,
                                               withModuleFromAST)
import           LLVM.General.PassManager     (PassSetSpec (..),
                                               defaultCuratedPassSetSpec,
                                               withPassManager)

foreign import ccall "dynamic"
    llvmToHaskellFunction :: FunPtr (IO ()) -> (IO ())

runForeign :: FunPtr a -> IO ()
runForeign fun = llvmToHaskellFunction $ castFunPtr fun

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit context = EE.withMCJIT context optlevel model ptrelim fastins
  where
    optlevel = Just 0   -- optimization level
    model    = Nothing  -- code model ( Default )
    ptrelim  = Nothing  -- frame pointer elimination
    fastins  = Nothing  -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT compiledModule = do
  withContext $ \context ->
      jit context $ \executionEngine ->
          runExceptT $ withModuleFromAST context compiledModule $ \m ->  -- TODO: handle errors properly
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

moduleToString :: AST.Module -> IO String
moduleToString m = withContext $ \context ->
    liftError $ withModuleFromAST context m moduleLLVMAssembly