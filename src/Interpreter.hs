{-# LANGUAGE LambdaCase #-}
module Interpreter (
    runInlineScript,
    runInlineScript',
    runScriptFile,
    evaluate
) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as MB
import Data.Function ( on )
import Control.Monad.State
import Control.Monad.Except
import Control.Exception ( try )
import Utils
import Builtins
import Tokenizer ( tokenize' )
import Parser ( parse )

_curryCall :: [AST] -> LFunction -> LContext AST
_curryCall [] f = return $ (makeNonsenseAST $ ASTFunction f)
_curryCall (arg:[]) f = f arg
_curryCall (arg:rest) f = do
    g <- _curryCall rest f
    case astNode g of
        ASTFunction f' -> f' arg
        other -> throwL (astPos g) $ "cannot call value " ++ show other ++ " as a function"

curryCall :: [AST] -> LFunction -> LContext AST
curryCall [] f = f (makeNonsenseAST ASTUnit)
curryCall args f = _curryCall args f

traverseAndReplace :: String -> AST -> AST -> AST
traverseAndReplace param arg ast@AST { astNode = ASTSymbol sym }
    | sym == param = arg
    | otherwise = ast
traverseAndReplace param arg ast@AST { astNode = ASTFunctionCall body } =
    ast { astNode = ASTFunctionCall $ (map (traverseAndReplace param arg) body) }
traverseAndReplace param arg ast@AST { astNode = ASTVector vec } =
    ast { astNode = ASTVector $ (map (traverseAndReplace param arg) vec) }
traverseAndReplace param arg ast@AST { astNode = ASTHashMap hmap } =
    ast { astNode = ASTHashMap $ M.assocs hmap
        $> L.concatMap (\(a, b) -> [a, b])
        .> map (traverseAndReplace param arg)
        .> asPairs .> M.fromList }
traverseAndReplace _ _ other = other

traverseAndRenameSymbol :: String -> String -> AST -> AST
traverseAndRenameSymbol name1 name2 ast@AST { astNode = ASTSymbol sym }
    | sym == name1 = ast { astNode = ASTSymbol name2 }
    | otherwise = ast
traverseAndRenameSymbol name1 name2 ast@AST { astNode = ASTFunctionCall body } =
    ast { astNode = ASTFunctionCall $ (map (traverseAndRenameSymbol name1 name2) body) }
traverseAndRenameSymbol name1 name2 ast@AST { astNode = ASTVector vec } =
    ast { astNode = ASTVector $ (map (traverseAndRenameSymbol name1 name2) vec) }
traverseAndRenameSymbol name1 name2 ast@AST { astNode = ASTHashMap hmap } =
    ast { astNode = ASTHashMap $ M.assocs hmap
        $> L.concatMap (\(a, b) -> [a, b])
        .> map (traverseAndRenameSymbol name1 name2)
        .> asPairs .> M.fromList }
traverseAndRenameSymbol _ _ other = other

foldSymValPairs :: [(String, AST)] -> AST -> AST
foldSymValPairs [] body = body
foldSymValPairs ((sym, val):rest) body =
    let replacedRestVals = map (snd .> traverseAndReplace sym val) rest
        replacedRest = zip (map fst rest) (replacedRestVals)
        replacedBody = traverseAndReplace sym val body
     in foldSymValPairs replacedRest replacedBody

letArgsToSymValPairs :: [AST] -> LContext (String, AST)
letArgsToSymValPairs args =
    case args of
        [AST { astNode = ASTSymbol symbol' }, value'] -> do
            evaledValue <- evaluate value'
            return (symbol', evaledValue)
        [AST { astNode = ASTSymbol "lazy" }, AST { astNode = ASTSymbol symbol' }, value'] -> do
            return (symbol', value')
        other -> throwL (astPos $ head other) $ "let! called with invalid args " ++ show other

defineUserFunction :: AST -> [AST] -> LContext LFunction
defineUserFunction paramAst@AST { astNode = ASTSymbol param } exprs = return fn where
    fn :: LFunction
    fn arg =
        let ret = do
                let replacedExprs = map (traverseAndReplace param arg) exprs
                let letExprs = take (length exprs - 1) replacedExprs
                letSymValPairs <- letExprs
                        $> mapM (\case AST { astNode = ASTFunctionCall v } -> return $ drop 1 v
                                       ast -> throwL (astPos ast) $ "unreachable: map letExprs, ast: " ++ show ast)
                        .> fmap (mapM $ letArgsToSymValPairs) .> join
                let body = head $ drop (length exprs - 1) replacedExprs
                let newBody = traverseAndReplace param arg body
                        $> foldSymValPairs letSymValPairs
                evaluate newBody
         in ret `catchError`
            appendError ("in a function definition at " ++ astPos paramAst)

defineUserFunction param exprs = throwL (astPos param)
    $ "unreachable: defineUserFunction, param: " ++ show param ++ ", exprs: " ++ show exprs

defineUserFunctionWithLetExprs :: [AST] -> [AST] -> LContext LFunction
defineUserFunctionWithLetExprs [] exprs =
    -- the position info is nonsensical, but it should never get read anyway
    defineUserFunction (makeNonsenseAST $ ASTSymbol "unit") exprs
defineUserFunctionWithLetExprs (param:[]) exprs =
    defineUserFunction param exprs
defineUserFunctionWithLetExprs (AST { astNode = ASTSymbol param }:rest) exprs = return fn where
    fn :: LFunction
    fn arg = do
        let newExprs = map (traverseAndReplace param arg) exprs
        ret <- defineUserFunctionWithLetExprs rest newExprs
        -- the returned AST will not have the correct position info, but that's fine
        -- because the info is overridden in evaluateFunctionDef anyway
        return $ makeNonsenseAST $ ASTFunction $ ret
defineUserFunctionWithLetExprs (param:_) _ = throwL (astPos $ param)
    $ "unreachable: defineUserFunctionWithLetExprs, param: " ++ show param

evaluateFunctionDef :: [AST] -> LContext AST
evaluateFunctionDef asts = do
    let defAst = head asts
        args = tail asts
    (params'', exprs) <- case args of
        args'
            | length args' < 2 ->
                throwL (astPos defAst) $ "\\ called with " ++ show (length args) ++ " arguments"
            | otherwise -> return $ (head args', tail args')

    AST { astNode = ASTVector params' } <- assertIsASTVector params''
    params <- mapM assertIsASTSymbol params'

    env <- getEnv
    let isParamNameShadowing name = MB.isJust $ resolveSymbol name env

    let shadowingParamM = L.find (asSymbol .> isParamNameShadowing) params
    case shadowingParamM of
        Just shadowingParam -> throwL (astPos shadowingParam)
            $ "parameter is shadowing already defined symbol " ++ show (astNode shadowingParam)
        Nothing -> return ()

    let letExprs = take (length exprs - 1) exprs
    let nonLetExprM = L.find (not . isLetAST) letExprs
    case nonLetExprM of
        Just nonLetExpr -> throwL (astPos nonLetExpr)
            $ "non-let expression in function definition before body: " ++ show nonLetExpr
        Nothing -> return ()

    fn <- defineUserFunctionWithLetExprs params exprs
    return $ defAst { astNode = ASTFunction fn }
    where
        isLetAST AST { astNode = ASTFunctionCall (AST { astNode = ASTSymbol "let!" }:_) } = True
        isLetAST _ = False
        asSymbol AST { astNode = ASTSymbol sym } = sym
        asSymbol ast = error $ "unreachable: evaluateFunctionDef asSymbol, ast: " ++ show ast

evaluateMatch :: [AST] -> LContext AST
evaluateMatch asts = do
    let matchAst = head asts
        args = tail asts

    (actualExpr, rest) <- case args of
        [] -> throwL (astPos matchAst) $ "match called with no arguments"
        (_:[]) -> throwL (astPos matchAst) "empty match cases"
        (a:b) -> return (a, b)

    pairs <- (asPairsM rest) `catchError`
                (\_ -> throwL (astPos matchAst) $ "invalid number of arguments passed to match\n"
                                ++ "- matching on expr: " ++ show actualExpr ++ "\n"
                                ++ "- arguments: " ++ show rest)

    evaledActual <- evaluate actualExpr
    ret <- matchPairs (actualExpr, evaledActual) pairs
    return $ matchAst { astNode = astNode ret }
    where
        matchPairs :: (AST, AST) -> [(AST, AST)] -> LContext AST
        matchPairs (actualExpr, evaledActual) [] = throwL (astPos actualExpr)
            $ "matching case not found when matching on expression: " ++ show actualExpr
            ++ " (actual value: " ++ show evaledActual ++ ")"
        matchPairs (actualExpr, evaledActual) ((matcher, branch):restPairs) = do
            evaledMatcher <- evaluate matcher
            if evaledActual == evaledMatcher
                then evaluate branch
                else matchPairs (actualExpr, evaledActual) restPairs

evaluateLet :: [AST] -> LContext AST
evaluateLet asts = do
    let letAst = head asts
        args = tail asts

    d <- getDepth
    when (d > 1) $ throwL (astPos letAst) $ "let! can only be called on the top level or in a function definition, current depth: " ++ show d

    (symbol, value) <- letArgsToSymValPairs args
    env <- getEnv
    when (MB.isJust $ resolveSymbol symbol env) $ throwL (astPos letAst) $ "symbol already defined: " ++ symbol
    insertThisEnv symbol value
    return $ letAst { astNode = ASTUnit }

evaluateEnv :: [AST] -> LContext AST
evaluateEnv asts = do
    let envAst = head asts

    d <- getDepth
    when (d > 1) $ throwL (astPos envAst) $ "env! can only be called on the top level, current depth: " ++ show d

    env <- getEnv
    let pairs = M.assocs (M.union (envThis env) (envImported env))
    let longestKey = L.maximumBy (compare `on` (length . fst)) pairs $> fst
    let pad s = s ++ take (length longestKey + 4 - length s) (L.repeat ' ')
    let rows = pairs $> map (\(k, v) -> pad k ++ show v)
    liftIO $ mapM_ putStrLn rows
    return $ envAst { astNode = ASTUnit }

evaluateImport :: [AST] -> LContext AST
evaluateImport asts = do
    let importAst = head asts
        args = tail asts

    d <- getDepth
    when (d > 1) $ throwL (astPos importAst) $ "import! can only be called on the top level, current depth: " ++ show d

    config <- getConfig
    let initialState = LState {
        stateConfig = config,
        stateDepth = 0,
        stateEnv = emptyEnv { envImported = builtinEnv }
    }
    case args of
        -- qualified import
        [AST { astNode = ASTSymbol qualifier }, AST { astNode = ASTString path }] -> do
            LState { stateEnv = evaledRawEnv } <- lift $ execStateT (runScriptFile path) initialState
            let exportedEnvMap = envThis evaledRawEnv

            let mangle k = qualifier ++ ":" ++ k
            let mangledExportsMap = M.mapKeys mangle exportedEnvMap
            let nonMangledKeys = M.keys exportedEnvMap
            let callWithAll (f:fs) x = callWithAll fs (f x)
                callWithAll [] x = x
            let traverseAndReplaceAllKeys = map (\k -> traverseAndRenameSymbol k (mangle k)) nonMangledKeys
            let mangledTraversedMap = M.map (\a -> (callWithAll traverseAndReplaceAllKeys a)) mangledExportsMap
            env <- getEnv
            let importedEnv = envImported env
            putImportedEnv $ M.union importedEnv mangledTraversedMap
            return $ importAst { astNode = ASTUnit }

        -- non-qualified import
        [AST { astNode = ASTString path }] -> do
            LState { stateEnv = evaledRawEnv } <- lift $ execStateT (runScriptFile path) initialState
            let exportedEnvMap = envThis evaledRawEnv

            env <- getEnv
            let importedEnv = envImported env
            putImportedEnv $ M.union importedEnv exportedEnvMap
            return $ importAst { astNode = ASTUnit }

        _ -> throwL (astPos importAst) $ "invalid arguments passed to import!: " ++ show args

evaluateUserFunction :: [AST] -> LContext AST
evaluateUserFunction children = do
    let fnAst = head children
        args = tail children
    fnEvaled <- evaluate fnAst
    AST { astNode = (ASTFunction fn) } <- assertIsASTFunction fnEvaled
    evaledArgs <- mapM evaluate args
    doubleEvaledArgs <- mapM evaluate evaledArgs

    result <- curryCall (reverse doubleEvaledArgs) fn

    -- todo: maybe remove double eval here? can't remember why it was added
    return $ fnAst { astNode = astNode result }

resolveSymbol :: String -> Env -> Maybe AST
resolveSymbol sym Env { envThis = envThis, envImported = envImported }
    | (MB.isJust $ thisValM) = thisValM
    | (MB.isJust $ importedValM) = importedValM
    | otherwise = Nothing
    where
        thisValM = M.lookup sym envThis
        importedValM = M.lookup sym envImported

evaluateSymbol :: AST -> LContext AST
evaluateSymbol ast@AST { astNode = ASTSymbol sym } = do
    env <- getEnv
    let val = resolveSymbol sym env
    case val of
        Just ast' -> return ast'
        Nothing -> throwL (astPos ast) $ "symbol " ++ sym ++ " not defined in environment"
evaluateSymbol ast = throwL (astPos ast) $ "unreachable: evaluateSymbol, ast: " ++ show ast

evaluate :: AST -> LContext AST
evaluate ast@AST { astNode = fnc@(ASTFunctionCall args@(x:_)) } =
     do config <- getConfig
        when (configPrintCallStack config) $ liftIO $ putStrLn $ "fn call: " ++ show fnc
        incrementDepth
        let task = case astNode x of
            -- remember to add these as reseved keywords in Builtins!
                ASTSymbol "\\" ->
                    evaluateFunctionDef args
                ASTSymbol "match" ->
                    evaluateMatch args
                ASTSymbol "let!" ->
                    evaluateLet args
                ASTSymbol "env!" ->
                    evaluateEnv args
                ASTSymbol "import!" ->
                    evaluateImport args
                _ ->
                    evaluateUserFunction args

        ret <- task `catchError`
            appendError ("when calling function " ++ show x ++ " at " ++ astPos ast)
        decrementDepth
        return ret
evaluate ast@AST { astNode = (ASTSymbol _) } =
    evaluateSymbol ast
evaluate ast@AST { astNode = (ASTVector vec) } =
     do incrementDepth
        rets <- mapM evaluate vec `catchError`
                    appendError ("when evaluating elements of vector " ++ show vec ++ " at " ++ astPos ast)
        decrementDepth
        return $ ast { astNode = ASTVector rets }
evaluate other =
    return other

-- LIB

runScriptFile :: String -> LContext [AST]
runScriptFile fileName = do
    let srcM :: IO (Either IOError String)
        srcM = try $ readFile fileName

    srcM' <- liftIO srcM
    src <- case srcM' of
        Right s -> return s
        Left _ -> throwL "" $ "Failed to open file: \"" ++ fileName ++ "\""
    runInlineScript fileName src

runInlineScript :: String -> String -> LContext [AST]
runInlineScript fileName src =
    runInlineScript' 1 fileName src

runInlineScript' :: LineNo -> String -> String -> LContext [AST]
runInlineScript' lineNo fileName src = do
    tokenized <- tokenize' (lineNo, 1) fileName src
    LState { stateConfig = config } <- get
    when (configVerboseMode config) $ liftIO $ putStrLn $ "tokenized:\t\t" ++ show tokenized
    parsed <- parse tokenized

    when (configVerboseMode config) $ do
        let output = "parsed:\t\t\t" ++ (map show parsed $> L.intercalate "\n\t\t\t")
        liftIO $ putStrLn output

    evaluated <- foldEvaluate parsed
    when (configPrintEvaled config) $ do
        liftIO $ mapM_ putStrLn (map show evaluated)

    return evaluated
        where
            foldEvaluate :: [AST] -> LContext [AST]
            foldEvaluate [] = return []
            foldEvaluate (ast:rest) = do
                newAst <- evaluate ast
                restEvaled <- foldEvaluate rest
                return $ newAst : restEvaled