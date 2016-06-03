{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rogue.LLVM.Codegen
    (
      -- * The @LLVM@ type and functions for global definitions
      LLVM
    , runLLVM
    , declareExternal
    , defineFunction
    , defineIOStrVariable

    -- * Function to convert 'Rogue.Parser.Tokens.TypeToken' to LLVM types
    , typeFromToken

    -- * The @Codegen@ type for handling blocks and state
    , Codegen (..)
    , execCodegen
    , addBlock
    , createBlocks
    , setBlock
    , entryBlockName
    , updateNameMap
    , lookupSymbolTable
    , externf
    , emptyModule
    , assign

    -- * Binary operations on operands

    -- Logic operations
    , land
    , lor
    , lnot

    -- Comparison operations
    , ieq
    , ineq
    , ileq
    , igeq
    , ilt
    , igt

    -- Math Operations
    , iadd
    , isub
    , ineg
    , imul
    , idiv
    , imod
    , ipow

    -- * Memory allocation operations
    , call
    , alloca
    , store
    , load

    -- * Control flow operations
    , br
    , cbr
    , ret
    ) where

import           Control.Monad.State                (MonadState, State,
                                                     execState, gets, modify)

import           Data.Char                          (ord)
import           Data.List                          (sortOn)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.String                        (IsString (..))

import           LLVM.General.AST                   (BasicBlock (..),
                                                     Definition (..),
                                                     Instruction (Add, Alloca, And, Call, ICmp, Load, Mul, Or, SDiv, SRem, Store, Sub, Xor),
                                                     Module (..), Name (..),
                                                     Named (..), Operand (..),
                                                     Parameter (Parameter),
                                                     Terminator (..), Type,
                                                     defaultModule)
import qualified LLVM.General.AST                   as AST
import           LLVM.General.AST.Global            (Global (..),
                                                     functionDefaults,
                                                     globalVariableDefaults)
import qualified LLVM.General.AST.Type              as T

import qualified LLVM.General.AST.Attribute         as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
import qualified LLVM.General.AST.IntegerPredicate  as IP

import           Rogue.Parser.Tokens                (Identifier, TypeToken (..))

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
    deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: Identifier -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefinition :: Definition -> LLVM ()
addDefinition newDef = do
    oldDefinitions <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = oldDefinitions ++ [newDef] }

defineFunction :: Identifier -> [(Name, Type)] -> Type -> [BasicBlock] -> LLVM ()
defineFunction funName argTypes retty body = addDefinition $
    GlobalDefinition $ functionDefaults {
      name        = Name funName
    , parameters  = ([Parameter ty nm [] | (nm, ty) <- argTypes], False)
    , returnType  = retty
    , basicBlocks = body
    }

declareExternal :: Identifier -> Type -> [(Name, Type)] -> LLVM ()
declareExternal funName retty argTypes = addDefinition $
  GlobalDefinition $ functionDefaults {
    name        = Name funName
  , parameters  = ([Parameter ty nm [] | (nm, ty) <- argTypes], True)
  , returnType  = retty
  , basicBlocks = []
  }

defineIOStrVariable :: Identifier -> String -> LLVM ()
defineIOStrVariable varName formatString = addDefinition $
    GlobalDefinition $ globalVariableDefaults {
      name        = Name varName
    , type'       = T.ArrayType (fromIntegral $ length formatString) T.i8
    , isConstant  = True
    , initializer = Just $ C.Array T.i8 $ map (C.Int 8 . fromIntegral . ord) formatString
    }

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

typeFromToken :: TypeToken -> T.Type
typeFromToken TokenBoolType = T.i1
typeFromToken TokenIntType  = T.i32
typeFromToken TokenUnitType = T.void

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(Identifier, Operand)]

data CodegenState
    = CodegenState {
      currentBlock :: Name                 -- Name of the active block to append to
    , blocks       :: Map Name BlockState  -- Blocks for function
    , symbolTable  :: SymbolTable          -- Function scope symbol table
    , blockCount   :: Int                  -- Count of basic blocks
    , count        :: Word                 -- Count of unnamed instructions
    , names        :: Names                -- Name Supply
    } deriving Show

data BlockState
    = BlockState {
      idx   :: Int                       -- Block index
    , stack :: [Named Instruction]       -- Stack of instructions
    , term  :: Maybe (Named Terminator)  -- Block terminator
    } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

-------------------------------------------------------------------------------
-- Names of codegens
-------------------------------------------------------------------------------

type Names = Map Identifier Int

uniqueName :: Identifier -> Names -> (Identifier, Names)
uniqueName nm ns = case Map.lookup nm ns of
                        Nothing -> (nm           , Map.insert nm 0        ns)
                        Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

updateNameMap :: Identifier -> Codegen ()
updateNameMap predefinedIdentifier = do
    oldNames <- gets names
    modify $ \codegenState -> codegenState { names = Map.insert predefinedIdentifier 0 oldNames }

instance IsString Name where
    fromString = Name . fromString

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortOn (idx . snd)

createBlocks :: CodegenState -> [BasicBlock]
createBlocks = map makeBlock . sortBlocks . Map.toList . blocks

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: Identifier
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \codegenState -> codegenState { count = i + 1 }
    return $ i + 1

addInstruction :: Name -> Instruction -> Codegen Operand
addInstruction name instruction = do
    curBlock  <- current
    let curStack = stack curBlock

    modifyBlock (curBlock { stack = curStack ++ [name := instruction] })
    return $ LocalReference T.i32 name  -- TODO: here should be actual type

namedInstruction :: Maybe Identifier -> Instruction -> Codegen Operand
namedInstruction (Just name) instruction = do
    identfiersNames <- gets names
    let (newName, newNameMap) = uniqueName name identfiersNames
    modify $ \codegenState -> codegenState { names = newNameMap }
    addInstruction (Name newName) instruction

namedInstruction Nothing instruction = do
    newNumber <- fresh
    let numberName = UnName newNumber
    addInstruction numberName instruction


terminator :: Named Terminator -> Codegen (Named Terminator)
terminator newTerminator = do
    curBlock <- current
    case term curBlock of
        Just oldTerminator -> return oldTerminator
        Nothing            -> do
            modifyBlock (curBlock { term = Just newTerminator })
            return newTerminator

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: Identifier -> Codegen Name
addBlock blockName = do
    bls <- gets blocks
    ix  <- gets blockCount
    nms <- gets names

    let new = emptyBlock ix
    let (qname, supply) = uniqueName blockName nms

    modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                     , blockCount = ix + 1
                     , names = supply
                     }

    return (Name qname)

setBlock :: Name -> Codegen Name
setBlock blockName = do
    modify $ \s -> s { currentBlock = blockName }
    return blockName

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
        Just x -> return x
        Nothing -> error $ "No such block: " ++ show c  -- TODO: monadic exception?

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: Identifier -> Operand -> Codegen ()
assign var x = do
    lcls <- gets symbolTable
    modify $ \s -> s { symbolTable = (var, x) : lcls }

lookupSymbolTable :: Identifier -> Codegen Operand
lookupSymbolTable var = do
    syms <- gets symbolTable
    case lookup var syms of
        Just x  -> return x
        Nothing -> error $ "Can't find local variable (not in scope): " ++ var  --- TODO: will be checked earlier?

-------------------------------------------------------------------------------
-- References
-------------------------------------------------------------------------------

global :: Name -> C.Constant
global = C.GlobalReference T.i32

externf :: Name -> Operand
externf = AST.ConstantOperand . global

-------------------------------------------------------------------------------
-- Arithmetic and Constants
-------------------------------------------------------------------------------

-- Logic operations
land :: Operand -> Operand -> Codegen Operand
land a b = namedInstruction Nothing $ And a b []

lor :: Operand -> Operand -> Codegen Operand
lor a b = namedInstruction Nothing $ Or a b []

lnot :: Operand -> Codegen Operand
lnot a = do
    let xorConst = AST.ConstantOperand $ C.Int 1 1
    namedInstruction Nothing $ Xor a xorConst []

-- Comparison operations
icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = namedInstruction Nothing $ ICmp cond a b []

ieq :: Operand -> Operand -> Codegen Operand
ieq = icmp IP.EQ

ineq :: Operand -> Operand -> Codegen Operand
ineq = icmp IP.NE

ileq :: Operand -> Operand -> Codegen Operand
ileq = icmp IP.SLE

igeq :: Operand -> Operand -> Codegen Operand
igeq = icmp IP.SGE

ilt :: Operand -> Operand -> Codegen Operand
ilt = icmp IP.SLT

igt :: Operand -> Operand -> Codegen Operand
igt = icmp IP.SGT

-- Math operations
-- imath :: Instruction -> Operand -> Operand -> Codegen Operand

iadd :: Operand -> Operand -> Codegen Operand
iadd a b = namedInstruction Nothing $ Add False False a b []

isub :: Operand -> Operand -> Codegen Operand
isub a b = namedInstruction Nothing $ Sub False False a b []

ineg :: Operand -> Codegen Operand
ineg a = do
    let zeroConst = AST.ConstantOperand $ C.Int 32 0
    isub zeroConst a

imul :: Operand -> Operand -> Codegen Operand
imul a b = namedInstruction Nothing $ Mul False False a b []

idiv :: Operand -> Operand -> Codegen Operand
idiv a b = namedInstruction Nothing $ SDiv False a b []

imod :: Operand -> Operand -> Codegen Operand
imod a b = namedInstruction Nothing $ SRem a b []

ipow :: Operand -> Operand -> Codegen Operand
ipow _ _ = error "Non implemented error"  -- TODO: implement

-------------------------------------------------------------------------------
-- Effects
-------------------------------------------------------------------------------
toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: Operand -> [Operand] -> Codegen Operand
call fn args = namedInstruction Nothing $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Identifier -> AST.Type -> Codegen Operand
alloca name ty = namedInstruction (Just name) $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = namedInstruction Nothing $ Store False ptr val Nothing 0 []

load :: Identifier -> Operand -> Codegen Operand
load name ptr = namedInstruction (Just name) $ Load False ptr Nothing 0 []

-------------------------------------------------------------------------------
-- Control Flow
-------------------------------------------------------------------------------
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Maybe Operand -> Codegen (Named Terminator)
ret retVal = terminator $ Do $ Ret retVal []