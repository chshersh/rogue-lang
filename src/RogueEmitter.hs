{-# LANGUAGE OverloadedStrings #-}

module RogueEmitter where

import           Control.Monad.State

-- import           Debug.Trace

import           Data.Bifunctor (bimap)
import           Data.Word
import           Data.Int
import           Control.Monad.Except
import           Control.Applicative
import           Data.Map (Map)
import qualified Data.Map                   as Map

import           LLVM.General.Module
import           LLVM.General.Context

import qualified LLVM.General.AST          as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type     as T

import           RogueTokens
import qualified RogueAST                   as S
import           RogueCodegen

toLLVMTypeSignature :: S.FunctionType -> [(AST.Name, AST.Type)]
toLLVMTypeSignature = map (bimap AST.Name typeFromToken)

codegenProgram :: S.Program -> LLVM ()
codegenProgram = mapM_ codegenTopLevel

codegenTopLevel :: S.Declaration -> LLVM ()
codegenTopLevel (S.VarDef _ _ _ _) = return ()  -- TODO: codegen global variables
codegenTopLevel (S.FunDef name arguments returnType funBody) = defineFunction name funLLVMArgs (typeFromToken returnType) funBlock
  where
    funLLVMArgs = toLLVMTypeSignature arguments
    funBlock    = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry

        forM arguments $ \(argName, argType) -> do
            let llvmType = typeFromToken argType
            let argPtrName = argName ++ "Arg"
            var <- alloca argPtrName llvmType  -- TODO: (return name with `ptr`) use type here?
            store var $ AST.LocalReference llvmType (AST.Name argName)
            updateNameMap argName
            assign argName var

        genBlockBody funBody

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------
genBlockBody :: S.Statements -> Codegen ()
genBlockBody []                           = ret Nothing          >> return ()
genBlockBody (res@(S.Ret _) :         []) = codegenStatement res >> return ()
genBlockBody (    (S.Ret _) :          _) = error "return expression not in the end of block (unreachable block)"
genBlockBody (innerStmt     : restInners) = codegenStatement innerStmt >> genBlockBody restInners

codegenStatement :: S.Statement -> Codegen ()
codegenStatement (S.Def (S.VarDef _ varName (Just varType) varValue)) = do -- TODO: no Just, ignore mutability for now
    let llvmVarType = typeFromToken varType
    var       <- alloca varName llvmVarType
    cgenedVal <- cgenExpr varValue
    store var cgenedVal
    assign varName var

codegenStatement (S.Exp fc@(S.VarOrCall _ _)) = cgenExpr fc >> return ()  -- TODO: make function stmt call not an instance of Expr

codegenStatement (S.Ass name val) = do
    resultVal  <- cgenExpr val
    varOperand <- lookupSymbolTable name
    store varOperand resultVal
    return ()

codegenStatement (S.If cond ifThen ifElse) = do
    ifThenBlock  <- addBlock "if.then"
    ifElseBlock  <- addBlock "if.else"
    ifMergeBlock <- addBlock "if.merge"

    -- cond
    cgenCond <- cgenExpr cond
    cbr cgenCond ifThenBlock ifElseBlock

    -- if-then block
    setBlock ifThenBlock
    genBlockBody ifThen
    br ifMergeBlock
    ifThenBlock <- getBlock

    -- if-else block
    setBlock ifElseBlock
    genBlockBody ifElse
    br ifMergeBlock
    ifElseBlock <- getBlock

    -- if-merge block
    setBlock ifMergeBlock
    return ()

codegenStatement (S.While cond whileTrue) = do
    whileBlock <- addBlock "while.loop"
    exitBlock  <- addBlock "while.end"

    -- cond
    cgenCond <- cgenExpr cond
    cbr cgenCond whileBlock exitBlock

    -- while-true
    setBlock whileBlock
    genBlockBody whileTrue
    cgenCond <- cgenExpr cond
    cbr cgenCond whileBlock exitBlock
    whileBlock <- getBlock

    -- exit block
    setBlock exitBlock
    return ()


codegenStatement (S.Ret Nothing)        = ret Nothing >> return ()
codegenStatement (S.Ret (Just retExpr)) = do
    retExprOperand <- cgenExpr retExpr
    ret $ Just retExprOperand
    return ()

codegenStatement st = error $ "This is not supposed to be here (not handled case in `codegenStatement`: " ++ show st

cgenExpr :: S.Expr -> Codegen AST.Operand
{- Constants and variable -}
cgenExpr (S.BoolConst b) = return $ AST.ConstantOperand $ C.Int 1  (fromIntegral $ fromEnum b)
cgenExpr (S.IntConst  n) = return $ AST.ConstantOperand $ C.Int 32 (fromIntegral n)
cgenExpr (S.VarExpr   x) = lookupSymbolTable x >>= load x

{- Binary operations -}
-- logical
cgenExpr (S.And a b) = cgenBinaryExpr (Map.lookup "&&" binaryOpMap) a b
cgenExpr (S.Or  a b) = cgenBinaryExpr (Map.lookup "||" binaryOpMap) a b
cgenExpr (S.Not a)   = cgenUnaryExpr  (Map.lookup "!"  unaryOpMap)  a

-- comparison
cgenExpr (S.Equal     a b) = cgenBinaryExpr (Map.lookup "==" binaryOpMap) a b
cgenExpr (S.NotEqual  a b) = cgenBinaryExpr (Map.lookup "!=" binaryOpMap) a b
cgenExpr (S.LowerEq   a b) = cgenBinaryExpr (Map.lookup "<=" binaryOpMap) a b
cgenExpr (S.Lower     a b) = cgenBinaryExpr (Map.lookup "<"  binaryOpMap) a b
cgenExpr (S.GreaterEq a b) = cgenBinaryExpr (Map.lookup ">=" binaryOpMap) a b
cgenExpr (S.Greater   a b) = cgenBinaryExpr (Map.lookup ">"  binaryOpMap) a b

-- arithmetics
cgenExpr (S.Plus     a b) = cgenBinaryExpr (Map.lookup "+"  binaryOpMap) a b
cgenExpr (S.Minus    a b) = cgenBinaryExpr (Map.lookup "-"  binaryOpMap) a b
cgenExpr (S.Times    a b) = cgenBinaryExpr (Map.lookup "*"  binaryOpMap) a b
cgenExpr (S.Division a b) = cgenBinaryExpr (Map.lookup "/"  binaryOpMap) a b
cgenExpr (S.Modulo   a b) = cgenBinaryExpr (Map.lookup "%"  binaryOpMap) a b
cgenExpr (S.Power    a b) = cgenBinaryExpr (Map.lookup "**" binaryOpMap) a b
cgenExpr (S.Negate   a)   = cgenUnaryExpr  (Map.lookup "-"  unaryOpMap)  a

{- Codegen function call -}
cgenExpr (S.VarOrCall funName arguments) = do
    llvmArguments <- mapM cgenExpr arguments
    call (externf (AST.Name funName)) llvmArguments

-- | Convenient function to generate all binary operations.
cgenBinaryExpr :: Maybe (AST.Operand -> AST.Operand -> Codegen AST.Operand) -> S.Expr -> S.Expr -> Codegen AST.Operand
cgenBinaryExpr (Just binaryOp) a b = do
    ca <- cgenExpr a
    cb <- cgenExpr b
    binaryOp ca cb

cgenUnaryExpr :: Maybe (AST.Operand -> Codegen AST.Operand) -> S.Expr -> Codegen AST.Operand
cgenUnaryExpr (Just unaryOp) a = cgenExpr a >>= unaryOp

binaryOpMap :: Map String (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binaryOpMap = Map.fromList [
        ("&&", land),
        ("||", lor),

        ("==", ieq),
        ("!=", ineq),
        ("<=", ileq),
        ("<",  ilt),
        (">=", igeq),
        (">",  igt),

        ("+",  iadd),
        ("-",  isub),
        ("*",  imul),
        ("/",  idiv),
        ("%",  imod),
        ("**", ipow)
    ]

unaryOpMap :: Map String (AST.Operand -> Codegen AST.Operand)
unaryOpMap = Map.fromList [
        ("!", lnot),
        ("-", ineg)
    ]

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

initModule :: AST.Module
initModule = emptyModule "my first module"

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegenLLVM :: AST.Module -> S.Program -> IO AST.Module
codegenLLVM givenModule program = withContext $ \context ->
    liftError $ withModuleFromAST context llvmAST $ \m -> do
        llstr <- moduleLLVMAssembly m
        putStrLn llstr
        return llvmAST
  where
    generatedLLVM = codegenProgram program
    llvmAST       = runLLVM givenModule generatedLLVM