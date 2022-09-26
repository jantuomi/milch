{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module TestUtils where

import Utils

testConfig :: Config
testConfig = Config {
    configScriptFileName = Nothing,
    configVerboseMode = False,
    configShowHelp = False,
    configPrintEvaled = False
}

testRunL :: LContext a -> IO (Either LException a)
testRunL = runL testConfig

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
