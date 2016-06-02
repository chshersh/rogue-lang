{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Rogue.Parser.ParserMonad
    ( ParserM (..)
    , ParserState (..)
    , column
    , fileName
    , inputStream
    , lineNumber
    , reportError
    ) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State

import Rogue.Parser.Tokens (Identifier)

data ParserState = ParserState
    { _fileName    :: Identifier
    , _inputStream :: String
    , _lineNumber  :: Int
    , _column      :: Int
    } deriving Show

makeLenses ''ParserState

newtype ParserM a = ParserM { unParserM :: StateT ParserState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState ParserState, MonadError String)

reportError :: String -> ParserM a
reportError message = do
    ParserState {..} <- get
    throwError $ "error in " ++ _fileName ++ ":" ++ show _lineNumber ++ ":" ++ show _column ++ " = " ++ message