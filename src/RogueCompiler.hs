module RogueCompiler where

import RogueLexer 
import RogueAST
import RogueParser
import RogueCodegen

-- | Check next kinds of semantic correctness:
-- * variables are used after their definition  (maybe not on global level),
-- * convert possible function calls to variables,
-- * check mutability,
-- * number of arguments passing to function.
checkSemantics :: Program -> Program
checkSemantics = id

typecheck :: Program -> Program
typecheck = id

optimize :: Program -> Program
optimize = id

-- compile :: String -> String
-- compile = generateLLVM . optimize . typecheck . checkSemantics . parseRogue . lexer