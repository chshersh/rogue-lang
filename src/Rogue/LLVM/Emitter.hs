{-# LANGUAGE OverloadedStrings #-}

module Rogue.LLVM.Emitter where

import           Control.Monad.State

import           Data.Bifunctor (bimap)
import           Data.Word
import           Data.Int
import           Control.Monad.Except
import           Control.Applicative
import           Data.Map (Map)
import qualified Data.Map                  as Map

import           LLVM.General.Module
import           LLVM.General.Context

import qualified LLVM.General.AST          as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type     as T

import           Rogue.Parser.Tokens
import qualified Rogue.AST.Untyped         as S
import           Rogue.LLVM.Codegen

toLLVMTypeSignature :: S.FunctionType -> [(AST.Name, AST.Type)]
toLLVMTypeSignature = map (bimap AST.Name typeFromToken)

codegenProgram :: S.Program -> LLVM ()
codegenProgram program = do
    defineIOStrVariable ".scanf_str"  "%d\0"
    defineIOStrVariable ".printf_str" "%d\n\0"

    mapM_ codegenTopLevel program

    declareExternal "scanf"  T.i32 [(AST.Name "", T.ptr T.i8)]
    declareExternal "printf" T.i32 [(AST.Name "", T.ptr T.i8)]

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

        genBlockBody True funBody

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------
genBlockBody :: Bool -> S.Statements -> Codegen ()
genBlockBody True  []                           = ret Nothing >> return ()
genBlockBody False []                           = return ()
genBlockBody _     (res@(S.Ret _) :         []) = codegenStatement res
genBlockBody _     (    (S.Ret _) :          _) = error "return expression not in the end of block (unreachable block)"
genBlockBody isFun (innerStmt     : restInners) = codegenStatement innerStmt >> genBlockBody isFun restInners

codegenStatement :: S.Statement -> Codegen ()
codegenStatement (S.Def (S.VarDef _ varName (Just varType) varValue)) = do -- TODO: no Just, ignore mutability for now
    let llvmVarType = typeFromToken varType
    var       <- alloca varName llvmVarType
    cgenedVal <- cgenExpr varValue
    -- v@(AST.ConstantOperand (C.GetElementPtr _ (C.GlobalReference ptrType (AST.Name ".str")) _)) <- cgenExpr varValue
    -- var       <- alloca varName (T.ptr $ T.i8)
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
    genBlockBody False ifThen
    br ifMergeBlock

    -- if-else block
    setBlock ifElseBlock
    genBlockBody False ifElse
    br ifMergeBlock

    -- if-merge block
    setBlock ifMergeBlock
    return ()

codegenStatement (S.While cond whileTrue) = do
    condBlock  <- addBlock "while.cond"
    whileBlock <- addBlock "while.loop"
    exitBlock  <- addBlock "while.end"

    br condBlock

    -- while-cond
    setBlock condBlock
    cgenCond <- cgenExpr cond
    cbr cgenCond whileBlock exitBlock

    -- while-true
    setBlock whileBlock
    genBlockBody False whileTrue
    br condBlock

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
cgenExpr (S.VarOrCall "printf" arguments) = do
    llvmArguments <- mapM cgenExpr arguments
    let formatString = AST.ConstantOperand $ C.GetElementPtr True (C.GlobalReference (T.ArrayType 4 T.i8) (AST.Name ".printf_str")) [C.Int 32 0, C.Int 32 0]
    call (externf (AST.Name "printf")) (formatString : llvmArguments)

cgenExpr (S.VarOrCall "scanf" [S.VarExpr x]) = do
    varOperand <- lookupSymbolTable x
    let formatString = AST.ConstantOperand $ C.GetElementPtr True (C.GlobalReference (T.ArrayType 3 T.i8) (AST.Name ".scanf_str")) [C.Int 32 0, C.Int 32 0]
    call (externf (AST.Name "scanf")) [formatString, varOperand]

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

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegenLLVM :: Identifier -> S.Program -> AST.Module
codegenLLVM moduleName program = llvmAST
  where
    generatedLLVM  = codegenProgram program
    providedModule = emptyModule moduleName
    llvmAST        = runLLVM providedModule generatedLLVM