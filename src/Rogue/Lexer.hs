module Rogue.Lexer (lexer) where

import Data.Char (isAlpha, isSpace, isDigit, isAlphaNum)

import Rogue.Tokens

-- | Threaded Lexer for Rogue PL.
lexer :: (Token -> P a) -> P a
lexer continuation ('\n':cs) lineNumber = continuation (Sep TokenEOL) cs (lineNumber + 1)

{- Skip spaces and parse number or Identifier-}
lexer continuation s@(c:cs) lineNumber 
    | isSpace c              = lexer continuation cs lineNumber
    | isDigit c || isAlpha c = lexWord continuation s lineNumber

lexer continuation s lineNumber = 
    case s of
        {- End of input -}
        [] -> continuation (Sep TokenEOF) "" lineNumber

        {- Different separators -}
        ',':cs         -> continuation (Sep TokenComma    ) cs lineNumber
        ':':cs         -> continuation (Sep TokenColon    ) cs lineNumber
        ';':cs         -> continuation (Sep TokenSemicolon) cs lineNumber
        '-':'>':cs     -> continuation (Sep TokenTypeArrow) cs lineNumber
        '.':'.':'.':cs -> continuation (Sep TokenDots     ) cs lineNumber
    
        {- Comparator signs -}
        '=':'=':cs -> continuation (Cmp TokenEQ ) cs lineNumber
        '!':'=':cs -> continuation (Cmp TokenNEQ) cs lineNumber
        '<':'=':cs -> continuation (Cmp TokenLEQ) cs lineNumber
        '>':'=':cs -> continuation (Cmp TokenGEQ) cs lineNumber
        '<':cs     -> continuation (Cmp TokenLT ) cs lineNumber
        '>':cs     -> continuation (Cmp TokenGT ) cs lineNumber

        {- Assignment token, should be after `==` for not overlapping -}
        '=':cs -> continuation (Stmt TokenAssignment) cs lineNumber

        {- Skip code comments -}
        '/':'/':commentText -> lexer continuation (dropWhile (/= '\n') commentText) lineNumber
        '/':'*':commentText -> lexer continuation (dropBlockComment commentText) lineNumber

        {- Math signs -}
        '+':cs     -> continuation (Math TokenPlus ) cs lineNumber
        '*':'*':cs -> continuation (Math TokenPow  ) cs lineNumber
        '*':cs     -> continuation (Math TokenTimes) cs lineNumber
        '/':cs     -> continuation (Math TokenDiv  ) cs lineNumber
        '%':cs     -> continuation (Math TokenMod  ) cs lineNumber
        '-':cs     -> continuation (Math TokenMinus) cs lineNumber

        {- Logical operations -}
        '&':'&':cs -> continuation (Logic TokenAnd) cs lineNumber
        '|':'|':cs -> continuation (Logic TokenOr ) cs lineNumber
        '|':cs     -> continuation (Sep TokenGuard) cs lineNumber  -- this is here for not overlapping
        '!':cs     -> continuation (Logic TokenNot) cs lineNumber  -- should be after !=

        {- Math Braces -}
        '(':cs -> continuation (Braces TokenRoundOP) cs lineNumber
        ')':cs -> continuation (Braces TokenRoundCL) cs lineNumber
        '{':cs -> continuation (Braces TokenCurlyOP) cs lineNumber
        '}':cs -> continuation (Braces TokenCurlyCL) cs lineNumber

        {- Unknown symbol -}
        _ -> Failed $ "Unknown symbol: " ++ [head s]
  where
    dropBlockComment ('*':'/':cs)    = cs
    dropBlockComment (_:commentText) = dropBlockComment commentText
    dropBlockComment []              = error "Not closed block comment"  -- TODO: more correct handling (monadic exception)

-- | Function to parse keyword, number or Identifier.
lexWord :: (Token -> P a) -> P a
lexWord continuation s lineNumber = 
    let (word, rest) = span isAlphaNum s
    in continuation (tokenize word) rest lineNumber

-- | Convert word to token.
tokenize :: Identifier -> Token

{- VarToken -}
tokenize "let" = Mut TokenLet
tokenize "mut" = Mut TokenMut

{- Boolean value -}
tokenize "false" = Val $ TokenBool False
tokenize "true"  = Val $ TokenBool True

{- TypeToken -}
tokenize "Bool" = Ty TokenBoolType
tokenize "Int"  = Ty TokenIntType
tokenize "Unit" = Ty TokenUnitType

{- Statement token (empty) -}
tokenize "skip" = Stmt TokenSkip

{- Data controlflow keywords -}
tokenize "if"     = Flow TokenIf
tokenize "else"   = Flow TokenElse
tokenize "while"  = Flow TokenWhile
tokenize "return" = Flow TokenReturn

{- Number or Identifier -}
tokenize s@(c:_)
    | all isDigit s = Val $ TokenInt $ read s
    | isAlpha c     = Var s

{- Unknown case, leads to error -}
tokenize s = error "This should never happen"