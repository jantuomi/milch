{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module TestUtils where

import Control.Monad.Except
import Control.Monad.Reader
import Utils

initialConfig :: Config
initialConfig = Config {
    configScriptFileName = Nothing,
    configVerboseMode = False,
    configShowHelp = False
}

runLContext :: LContext a -> IO a
runLContext lc = do res <- runExceptT $ runReaderT lc initialConfig
                    case res of
                        Left (LException err) -> error err
                        Right val -> return val
