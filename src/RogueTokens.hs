module RogueTokens where

data ParseResult a = Ok a | Failed String

type Identifier = String
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l ->
   case m s l of 
       Ok a -> k a s l
       Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l -> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l ->
   case m s l of
      Ok a -> Ok a
      Failed e -> k e s l

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
    deriving Show

data MutabilityToken
    = TokenLet  -- let
    | TokenMut  -- mut
    deriving Show

data ValueToken
    = TokenInt Int     -- 123
    | TokenBool Bool   -- `true` or `false` 
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
