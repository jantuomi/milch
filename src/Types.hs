module Types where

import Control.Monad.Except
import Control.Monad.Reader

newtype LException = LException String
data Config = Config {
    configScriptFileName :: Maybe String,
    configVerboseMode :: Bool,
    configShowHelp :: Bool
}

type LContext a = ReaderT Config (ExceptT LException IO) a
