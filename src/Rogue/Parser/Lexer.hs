module Rogue.Parser.Lexer (lexer) where

import Data.Char (isAlpha, isSpace, isDigit, isAlphaNum)

import Control.Lens
import Control.Monad.State

import Rogue.Parser.ParserMonad
import Rogue.Parser.Tokens

-- | Threaded Lexer for Rogue PL.
lexer :: (Token -> ParserM a) -> ParserM a
lexer continuation = do
    input    <- gets _inputStream
    curToken <- runLexer input
    continuation curToken
  
-- | Helper function to yield tokens.
runLexer :: String -> ParserM Token

{- End of input -}
runLexer [] = return $ Sep TokenEOF

{- Special case for line breaks -}
runLexer ('\n':rest) = endOfLineState rest

{- Skip spaces and parse number or Identifier-}
runLexer s@(c:cs)
    | isSpace c              = moveInputColumnWithToken cs 1 DummyToken >> runLexer cs
    | isDigit c || isAlpha c = lexWord s

{- Different separators -}
runLexer (',':cs        ) = moveInputColumnWithToken cs 1 $ Sep TokenComma
runLexer (':':cs        ) = moveInputColumnWithToken cs 1 $ Sep TokenColon
runLexer (';':cs        ) = moveInputColumnWithToken cs 1 $ Sep TokenSemicolon
runLexer ('-':'>':cs    ) = moveInputColumnWithToken cs 2 $ Sep TokenTypeArrow
runLexer ('.':'.':'.':cs) = moveInputColumnWithToken cs 3 $ Sep TokenDots

{- Comparator signs -}
runLexer ('=':'=':cs) = moveInputColumnWithToken cs 2 $ Cmp TokenEQ 
runLexer ('!':'=':cs) = moveInputColumnWithToken cs 2 $ Cmp TokenNEQ
runLexer ('<':'=':cs) = moveInputColumnWithToken cs 2 $ Cmp TokenLEQ
runLexer ('>':'=':cs) = moveInputColumnWithToken cs 2 $ Cmp TokenGEQ
runLexer ('<':cs    ) = moveInputColumnWithToken cs 1 $ Cmp TokenLT
runLexer ('>':cs    ) = moveInputColumnWithToken cs 1 $ Cmp TokenGT

{- Assignment token, should be after `==` for not overlapping -}
runLexer ('=':cs) = moveInputColumnWithToken cs 1 $ Stmt TokenAssignment

{- Skip code comments -}
runLexer ('/':'/':commentText) = runLexer $ dropWhile (/= '\n') commentText
runLexer ('/':'*':commentText) = dropBlockComment commentText >>= runLexer

{- Math signs -}
runLexer ('+':cs    ) = moveInputColumnWithToken cs 1 $ Math TokenPlus
runLexer ('*':'*':cs) = moveInputColumnWithToken cs 2 $ Math TokenPow
runLexer ('*':cs    ) = moveInputColumnWithToken cs 1 $ Math TokenTimes
runLexer ('/':cs    ) = moveInputColumnWithToken cs 1 $ Math TokenDiv
runLexer ('%':cs    ) = moveInputColumnWithToken cs 1 $ Math TokenMod
runLexer ('-':cs    ) = moveInputColumnWithToken cs 1 $ Math TokenMinus

{- Logical operations -}
runLexer ('&':'&':cs) = moveInputColumnWithToken cs 2 $ Logic TokenAnd
runLexer ('|':'|':cs) = moveInputColumnWithToken cs 2 $ Logic TokenOr
runLexer ('|':cs    ) = moveInputColumnWithToken cs 1 $ Sep TokenGuard  -- this is here for not overlapping
runLexer ('!':cs    ) = moveInputColumnWithToken cs 1 $ Logic TokenNot  -- should be after !=

{- Braces -}
runLexer ('(':cs) = moveInputColumnWithToken cs 1 $ Braces TokenRoundOP
runLexer (')':cs) = moveInputColumnWithToken cs 1 $ Braces TokenRoundCL
runLexer ('{':cs) = moveInputColumnWithToken cs 1 $ Braces TokenCurlyOP
runLexer ('}':cs) = moveInputColumnWithToken cs 1 $ Braces TokenCurlyCL

{- Unknown symbol -}
runLexer unknown = reportError $ "Unknown symbol: " ++ [head unknown]

-- | Function to parse keyword, number or Identifier.
lexWord :: String -> ParserM Token
lexWord input = do
    let (word, rest) = span isAlphaNum input
    let wordToken    = tokenize word

    case wordToken of
         DummyToken -> reportError $ "Unhandled case in lexical analysis: " ++ word
         _          -> moveInputColumnWithToken rest (length word) wordToken

dropBlockComment :: String -> ParserM String
dropBlockComment ('*':'/':ct) = moveInputColumnWithToken ct 2 DummyToken >> return ct
dropBlockComment ('\n':ct)    = endOfLineState ct                        >> dropBlockComment ct
dropBlockComment (_:ct)       = moveInputColumnWithToken ct 1 DummyToken >> dropBlockComment ct
dropBlockComment []           = reportError "Block comment not closed"

endOfLineState :: String -> ParserM Token
endOfLineState newLineInput = do
    lineNumber  += 1
    column      .= 0
    inputStream .= newLineInput
    return $ Sep TokenEOL

-- | Util function to handle parser state and return given token.
moveInputColumnWithToken :: String -> Int -> Token -> ParserM Token
moveInputColumnWithToken rest tokenLen token = do
    inputStream .= rest
    column      += tokenLen
    return token        

-- | Convert keyword to token.
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
tokenize _ = DummyToken