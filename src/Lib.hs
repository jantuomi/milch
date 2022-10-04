{-# LANGUAGE LambdaCase #-}
module Lib (
    runScriptFile,
    runInlineScript,
) where

import qualified Data.List as L
import Control.Monad.Except
import Control.Monad.Reader
import Utils
import Tokenizer ( tokenize )
import Parser ( parse )
import Evaluator ( evaluate )

runScriptFile :: Env -> String -> LContext (Env, [AST])
runScriptFile env fileName = do
    src <- liftIO $ readFile fileName
    runInlineScript fileName env src

runInlineScript :: String -> Env -> String -> LContext (Env, [AST])
runInlineScript fileName env src = do
    tokenized <- tokenize fileName src
    config <- ask
    when (configVerboseMode config) $ liftIO $ putStrLn $ "tokenized:\t\t" ++ show tokenized
    parsed <- parse tokenized

    when (configVerboseMode config) $ do
        let output = "parsed:\t\t\t" ++ (map show parsed $> L.intercalate "\n\t\t\t")
        liftIO $ putStrLn output

    (newEnv, evaluated) <- foldEvaluate env parsed

    when (configPrintEvaled config) $ do
        liftIO $ mapM_ putStrLn (map show evaluated)

    return (newEnv, evaluated)
        where
            foldEvaluate :: Env -> [AST] -> LContext (Env, [AST])
            foldEvaluate accEnv [] = return (accEnv, [])
            foldEvaluate accEnv (ast:rest) = do
                (newAccEnv, newAst) <- evaluate 0 accEnv ast
                (retEnv, restEvaled) <- foldEvaluate newAccEnv rest
                return $ (retEnv, newAst : restEvaled)
