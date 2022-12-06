{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module TestUtils where

import Utils

testConfig :: Config
testConfig = Config {
    configScriptFileName = Nothing,
    configVerboseMode = False,
    configShowHelp = False,
    configPrintEvaled = PrintEvaledOff,
    configPrintCallStack = False,
    configUseREPL = False
}

testRunL :: Env -> LContext a -> IO (Either LException (a, LState))
testRunL env = runL LState {
    stateConfig = testConfig,
    stateEnv = env,
    stateDepth = 0,
    statePure = Impure
}

expectSuccessL :: Env -> LContext a -> IO (a, LState)
expectSuccessL env lc =
     do res <- testRunL env lc
        case res of
            Left (LException _ err) -> error $ "unexpected error: " ++ err
            Right val -> return val

expectErrorL :: Show a => Env -> LContext a -> IO String
expectErrorL env lc =
     do res <- testRunL env lc
        case res of
            Left (LException _ err) -> return err
            Right (val, _) -> error $ "unexpected success: " ++ show val

ast :: ASTNode -> AST
ast node = AST { astNode = node }

astInteger a = ast $ ASTInteger a
astDouble a = ast $ ASTDouble a
astSymbol a = ast $ ASTSymbol a
astBoolean a = ast $ ASTBoolean a
astString a = ast $ ASTString a
astVector a = ast $ ASTVector a
astFunctionCall a = ast $ ASTFunctionCall a
astHashMap a = ast $ ASTHashMap a
astUnit = ast $ ASTUnit
astHole = ast $ ASTHole
