module RogueLexer (lexer) where

import Data.Char (isAlpha, isSpace, isDigit, isAlphaNum)

import RogueTokens

-- | Lexer for Rogue PL.
lexer :: String -> [Token]

{- End of input -}
-- lexer [] = [TokenEOF]
lexer [] = []

{- Different separators -}
lexer ('\n':cs)        = Sep TokenEOL       : lexer cs
lexer (',':cs)         = Sep TokenComma     : lexer cs
lexer (':':cs)         = Sep TokenColon     : lexer cs
lexer (';':cs)         = Sep TokenSemicolon : lexer cs
lexer ('-':'>':cs)     = Sep TokenTypeArrow : lexer cs
lexer ('.':'.':'.':cs) = Sep TokenDots      : lexer cs

{- Skip spaces and parse number or Identifier-}
lexer s@(c:cs) 
    | isSpace c = lexer cs
    | isDigit c || isAlpha c = lexWord s

{- Comparator signs -}
lexer ('=':'=':cs) = Cmp TokenEQ  : lexer cs
lexer ('!':'=':cs) = Cmp TokenNEQ : lexer cs
lexer ('<':'=':cs) = Cmp TokenLEQ : lexer cs
lexer ('>':'=':cs) = Cmp TokenGEQ : lexer cs
lexer ('<':cs)     = Cmp TokenLT  : lexer cs
lexer ('>':cs)     = Cmp TokenGT  : lexer cs

{- Assignment token, should be after `==` for not overlapping -}
lexer ('=':cs) = Stmt TokenAssignment : lexer cs

{- Skip code comments -}
lexer ('/':'/':commentText) = lexer $ dropWhile (/= '\n') commentText
lexer ('/':'*':commentText) = lexer $ dropBlockComment commentText
  where
    dropBlockComment ('*':'/':cs)    = cs
    dropBlockComment (_:commentText) = dropBlockComment commentText
    dropBlockComment []              = error "Not closed block comment"  -- TODO: more correct handling (monadic exception)

{- Math signs -}
lexer ('+':cs)     = Math TokenPlus  : lexer cs
lexer ('*':'*':cs) = Math TokenPow   : lexer cs
lexer ('*':cs)     = Math TokenTimes : lexer cs
lexer ('/':cs)     = Math TokenDiv   : lexer cs
lexer ('%':cs)     = Math TokenMod   : lexer cs
lexer ('-':cs)     = Math TokenMinus : lexer cs  -- TODO: handle case with negative numbers

{- Logical operations -}
lexer ('&':'&':cs) = Logic TokenAnd : lexer cs
lexer ('|':'|':cs) = Logic TokenOr  : lexer cs
lexer ('|':cs)     = Sep TokenGuard : lexer cs  -- this is here for not overlapping
lexer ('!':cs)     = Logic TokenNot : lexer cs  -- should be after !=

{- Math Braces -}
lexer ('(':cs) = Braces TokenRoundOP : lexer cs
lexer (')':cs) = Braces TokenRoundCL : lexer cs
lexer ('{':cs) = Braces TokenCurlyOP : lexer cs
lexer ('}':cs) = Braces TokenCurlyCL : lexer cs

-- | Function to parse keyword, number or Identifier.
lexWord :: String -> [Token]
lexWord s = let (word, rest) = span isAlphaNum s
            in tokenize word : lexer rest

-- | Convert word to token.
tokenize :: String -> Token

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
tokenize s = UnknownToken s