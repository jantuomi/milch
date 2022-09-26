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

testRunL :: LContext a -> IO (Either LException a)
testRunL lc = runExceptT $ runReaderT lc initialConfig

expectSuccessL :: LContext a -> IO a
expectSuccessL lc =
     do res <- testRunL lc
        case res of
            Left (LException err) -> error $ "unexpected error: " ++ err
            Right val -> return val

expectErrorL :: Show a => LContext a -> IO String
expectErrorL lc =
     do res <- testRunL lc
        case res of
            Left (LException err) -> return err
            Right val -> error $ "unexpected success: " ++ show val
