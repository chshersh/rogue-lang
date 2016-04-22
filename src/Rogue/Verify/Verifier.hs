{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Rogue.Verify.Verifier (verify) where

import           Data.HashSet (HashSet)
import qualified Data.HashSet                 as HS

import           Control.Applicative (liftA2)
import           Control.Monad.Reader

import           Rogue.AST.Untyped

-- | Check next kinds of semantic correctness:
-- * convert possible function calls to variables,
-- * TODO: variables are used after their definition  (maybe not on global level),
-- * TODO: number of arguments passing to function,
-- * TODO: check mutability.
verify :: Program -> Program
verify program = runReader (verifyTopLevel program) $ collectFunNames program $ HS.fromList ["scanf", "printf"]

-- | Collect all names of global function definitions.
collectFunNames :: Program -> HashSet String -> HashSet String
collectFunNames                           []  hashSet = hashSet
collectFunNames (FunDef { defName, .. } : ps) hashSet = collectFunNames ps $ HS.insert defName hashSet
collectFunNames (                     _ : ps) hashSet = collectFunNames ps hashSet
    
-- | Verify top level bindings such as Global variables and Global functions
verifyTopLevel :: Program -> Reader (HashSet String) Program
verifyTopLevel                               []  = return []
verifyTopLevel (   VarDef {          .. }  : ps) = verifyTopLevel ps  -- TODO: handle global variables
verifyTopLevel (f@(FunDef { funBody, .. }) : ps) = do
    validBody <- verifyBody funBody
    validRest <- verifyTopLevel ps
    return $ f { funBody = validBody } : validRest 

verifyBody :: Statements -> Reader (HashSet String) Statements
verifyBody [] = return []
verifyBody (Def v@(VarDef { varValue, .. }) : ss) = do
    validDefinition <- verifyExpr varValue
    validRest       <- verifyBody ss
    return $ Def (v { varValue = validDefinition }) : validRest

verifyBody (Exp e     : ss) = liftA2 (:) (Exp     <$> verifyExpr e) (verifyBody ss)
verifyBody (Ass var e : ss) = liftA2 (:) (Ass var <$> verifyExpr e) (verifyBody ss)
verifyBody (If cond ifTrue ifFalse : ss) = do
    validCond    <- verifyExpr cond
    validIfTrue  <- verifyBody ifTrue
    validIfFalse <- verifyBody ifFalse

    validRest    <- verifyBody ss

    return $ If validCond validIfTrue validIfFalse : validRest   

verifyBody (While cond whileBody : ss) = do
    validCond      <- verifyExpr cond
    validWhileBody <- verifyBody whileBody

    validRest      <- verifyBody ss

    return $ While validCond validWhileBody : validRest

verifyBody (Ret maybeExpr : ss) = do
    validRetExpr <- sequence $ verifyExpr <$> maybeExpr
    validRest    <- verifyBody ss  -- TODO: handle end of return here
    return $ Ret validRetExpr : validRest

-- | Verify expressions.
verifyExpr :: Expr -> Reader (HashSet String) Expr

{- Logic expressions -}
verifyExpr (And l r) = liftA2 And (verifyExpr l) (verifyExpr r)
verifyExpr (Or  l r) = liftA2 Or  (verifyExpr l) (verifyExpr r)
verifyExpr (Not e)   = Not <$> verifyExpr e

{- Comparison expressions -}
verifyExpr (Equal     l r) = liftA2 Equal     (verifyExpr l) (verifyExpr r)
verifyExpr (NotEqual  l r) = liftA2 NotEqual  (verifyExpr l) (verifyExpr r)
verifyExpr (LowerEq   l r) = liftA2 LowerEq   (verifyExpr l) (verifyExpr r)
verifyExpr (Lower     l r) = liftA2 Lower     (verifyExpr l) (verifyExpr r)
verifyExpr (GreaterEq l r) = liftA2 GreaterEq (verifyExpr l) (verifyExpr r)
verifyExpr (Greater   l r) = liftA2 Greater   (verifyExpr l) (verifyExpr r)

{- Math expressions -}
verifyExpr (Plus     l r) = liftA2 Plus     (verifyExpr l) (verifyExpr r)
verifyExpr (Minus    l r) = liftA2 Minus    (verifyExpr l) (verifyExpr r)
verifyExpr (Times    l r) = liftA2 Times    (verifyExpr l) (verifyExpr r)
verifyExpr (Division l r) = liftA2 Division (verifyExpr l) (verifyExpr r)
verifyExpr (Modulo   l r) = liftA2 Modulo   (verifyExpr l) (verifyExpr r)
verifyExpr (Power    l r) = liftA2 Power    (verifyExpr l) (verifyExpr r)
verifyExpr (Negate e)     = Negate <$> verifyExpr e

{- Constans, functions and variables -}
verifyExpr i@(IntConst  _) = return i
verifyExpr b@(BoolConst _) = return b
verifyExpr   (VarOrCall name args) = do
    globalFunNames <- ask
    if HS.member name globalFunNames then do
        validArgs <- mapM verifyExpr args
        return $ VarOrCall name validArgs
    else if not $ null args then
        error $ name ++ " is variable but applied to " ++ show args  -- TODO: monadic exception
    else 
        return $ VarExpr name

verifyExpr v@(VarExpr _) = error $ "WTF: " ++ show v