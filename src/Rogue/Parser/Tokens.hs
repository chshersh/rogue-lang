module Rogue.Parser.Tokens
    ( Identifier
    , Token            (..)
    , MutabilityToken  (..)
    , ValueToken       (..)
    , TypeToken        (..)
    , CmpToken         (..)
    , MathToken        (..)
    , LogicalToken     (..)
    , BracesToken      (..)
    , SeparateToken    (..)
    , StatementToken   (..)
    , ControlFlowToken (..)
    ) where

type Identifier = String

data Token
    = Mut    MutabilityToken
    | Val    ValueToken
    | Var    Identifier  -- x1
    | Ty     TypeToken
    | Cmp    CmpToken
    | Math   MathToken
    | Logic  LogicalToken
    | Braces BracesToken
    | Sep    SeparateToken
    | Stmt   StatementToken
    | Flow   ControlFlowToken
    | DummyToken
    deriving Show

data MutabilityToken
    = TokenLet  -- let
    | TokenMut  -- mut
    deriving Show

data ValueToken
    = TokenInt Int    -- 123
    | TokenBool Bool  -- `true` or `false`
    deriving Show

data TypeToken
    = TokenBoolType  -- Bool
    | TokenIntType   -- Int
    | TokenUnitType  -- Unit
    deriving Show

data CmpToken
    = TokenEQ   -- ==
    | TokenNEQ  -- !=
    | TokenLT   -- <
    | TokenGT   -- >
    | TokenLEQ  -- <=
    | TokenGEQ  -- >=
    deriving Show

data MathToken
    = TokenPlus   -- +
    | TokenMinus  -- -
    | TokenTimes  -- *
    | TokenDiv    -- /
    | TokenMod    -- %
    | TokenPow    -- **
    deriving Show

data LogicalToken
    = TokenAnd  -- &&
    | TokenOr   -- ||
    | TokenNot  -- !
    deriving Show

data BracesToken
    = TokenRoundOP -- (
    | TokenRoundCL -- )
    | TokenCurlyOP -- {
    | TokenCurlyCL -- }
    deriving Show

data SeparateToken
    = TokenComma      -- ,
    | TokenColon      -- :
    | TokenSemicolon  -- ;
    | TokenTypeArrow  -- ->
    | TokenGuard      -- |
    | TokenDots       -- ...
    | TokenEOL        -- \n
    | TokenEOF        -- <eof>
    deriving Show

data StatementToken
    = TokenAssignment  -- =
    | TokenSkip        -- skip
    deriving Show

data ControlFlowToken
    = TokenIf      -- if
    | TokenElse    -- else
    | TokenWhile   -- while
    | TokenReturn  -- return
    deriving Show
