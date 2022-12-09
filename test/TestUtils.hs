{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module TestUtils where

import qualified Data.Map as M
import qualified Data.Bifunctor as B
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
    statePure = Impure,
    stateAtomMap = M.empty
}

expectSuccessL :: Env -> LContext a -> IO (a, LState)
expectSuccessL env lc =
     do res <- testRunL env lc
        case res of
            Left ex -> error $ "unexpected error: " ++ (foldStackMessage ex)
            Right val -> return val

expectErrorL :: Show a => Env -> LContext a -> IO String
expectErrorL env lc =
     do res <- testRunL env lc
        case res of
            Left ex -> return $ foldStackMessage ex
            Right (val, _) -> error $ "unexpected success: " ++ show val

makeEnv :: [(String, AST)] -> Env
makeEnv = M.fromList . map (B.second Regular)

ast :: ASTNode -> AST
ast node = makeNonsenseAST node

astInteger :: Integer -> AST
astInteger a = ast $ ASTInteger a
astDouble :: Double -> AST
astDouble a = ast $ ASTDouble a
astSymbol :: String -> AST
astSymbol a = ast $ ASTSymbol a
astBoolean :: Bool -> AST
astBoolean a = ast $ ASTBoolean a
astString :: String -> AST
astString a = ast $ ASTString a
astTag :: String -> AST
astTag a = ast $ ASTTag (computeTagN a) a
astVector :: [AST] -> AST
astVector a = ast $ ASTVector a
astFunctionCall :: [AST] -> AST
astFunctionCall a = ast $ ASTFunctionCall a
astHashMap :: M.Map AST AST -> AST
astHashMap a = ast $ ASTHashMap a
astRecord :: String -> LRecord -> AST
astRecord a hmap = ast $ ASTRecord (computeTagN a) a hmap
astUnit :: AST
astUnit = ast $ ASTUnit
astHole :: AST
astHole = ast $ ASTHole
