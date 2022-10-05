{-# LANGUAGE LambdaCase #-}
module Interpreter (
    runInlineScript,
    runScriptFile,
    evaluate
) where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Function ( on )
import Control.Monad.State
import Control.Monad.Except
import Utils
import Builtins
import Tokenizer ( tokenize )
import Parser ( parse )

type Depth = Int

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

foldSymValPairs :: [(String, AST)] -> AST -> AST
foldSymValPairs [] body = body
foldSymValPairs ((sym, val):rest) body =
    let replacedRestVals = map (snd .> traverseAndReplace sym val) rest
        replacedRest = zip (map fst rest) (replacedRestVals)
        replacedBody = traverseAndReplace sym val body
     in foldSymValPairs replacedRest replacedBody

letArgsToSymValPairs :: Depth -> [AST] -> LContext (String, AST)
letArgsToSymValPairs d args =
    case args of
        [AST { astNode = ASTSymbol symbol' }, value'] -> do
            evaledValue <- evaluate d value'
            return (symbol', evaledValue)
        [AST { astNode = ASTSymbol "lazy" }, AST { astNode = ASTSymbol symbol' }, value'] -> do
            return (symbol', value')
        other -> throwL (astPos $ head other) $ "let! called with invalid args " ++ show other

defineUserFunction :: Depth -> AST -> [AST] -> LContext LFunction
defineUserFunction d AST { astNode = ASTSymbol param } exprs = return fn where
    fn :: LFunction
    fn arg = do
        let replacedExprs = map (traverseAndReplace param arg) exprs
        let letExprs = take (length exprs - 1) replacedExprs
        letSymValPairs <- letExprs
                $> mapM (\case AST { astNode = ASTFunctionCall v } -> return $ drop 1 v
                               ast -> throwL (astPos ast) $ "unreachable: map letExprs, ast: " ++ show ast)
                .> fmap (mapM $ letArgsToSymValPairs d) .> join
        let body = head $ drop (length exprs - 1) replacedExprs
        let newBody = traverseAndReplace param arg body
                $> foldSymValPairs letSymValPairs
        evaluate d newBody

defineUserFunction _ param exprs = throwL (astPos param)
    $ "unreachable: defineUserFunction, param: " ++ show param ++ ", exprs: " ++ show exprs

defineUserFunctionWithLetExprs :: Depth -> [AST] -> [AST] -> LContext LFunction
defineUserFunctionWithLetExprs d [] exprs =
    -- the position info is nonsensical, but it should never get read anyway
    defineUserFunction d (makeNonsenseAST $ ASTSymbol "unit") exprs
defineUserFunctionWithLetExprs d (param:[]) exprs =
    defineUserFunction d param exprs
defineUserFunctionWithLetExprs d (AST { astNode = ASTSymbol param }:rest) exprs = return fn where
    fn :: LFunction
    fn arg = do
        let newExprs = map (traverseAndReplace param arg) exprs
        ret <- defineUserFunctionWithLetExprs d rest newExprs
        -- the returned AST will not have the correct position info, but that's fine
        -- because the info is overridden in evaluateFunctionDef anyway
        return $ makeNonsenseAST $ ASTFunction $ ret
defineUserFunctionWithLetExprs _ (param:_) _ = throwL (astPos $ param)
    $ "unreachable: defineUserFunctionWithLetExprs, param: " ++ show param

evaluateFunctionDef :: Depth -> [AST] -> LContext AST
evaluateFunctionDef d asts = do
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
    let isParamNameShadowing name = M.member name env

    let shadowingParamM = L.find (astNode .> (\(ASTSymbol sym) -> sym) .> isParamNameShadowing) params
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

    fn <- defineUserFunctionWithLetExprs d params exprs
    return $ defAst { astNode = ASTFunction fn }
    where
        isLetAST AST { astNode = ASTFunctionCall (AST { astNode = ASTSymbol "let!" }:_) } = True
        isLetAST _ = False

evaluateMatch :: Depth -> [AST] -> LContext AST
evaluateMatch d asts = do
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

    evaledActual <- evaluate d actualExpr
    ret <- matchPairs (actualExpr, evaledActual) pairs
    return $ matchAst { astNode = astNode ret }
    where
        matchPairs :: (AST, AST) -> [(AST, AST)] -> LContext AST
        matchPairs (actualExpr, evaledActual) [] = throwL (astPos actualExpr)
            $ "matching case not found when matching on expression: " ++ show actualExpr
            ++ " (actual value: " ++ show evaledActual ++ ")"
        matchPairs (actualExpr, evaledActual) ((matcher, branch):restPairs) = do
            evaledMatcher <- evaluate d matcher
            if evaledActual == evaledMatcher
                then evaluate d branch
                else matchPairs (actualExpr, evaledActual) restPairs

evaluateLet :: Depth -> [AST] -> LContext AST
evaluateLet d asts = do
    let letAst = head asts
        args = tail asts
    when (d > 1) $ throwL (astPos letAst) $ "let! can only be called on the top level or in a function definition"
    (symbol, value) <- letArgsToSymValPairs d args
    env <- getEnv
    when (M.member symbol env) $ throwL (astPos letAst) $ "symbol already defined: " ++ symbol
    insertEnv symbol value
    return $ letAst { astNode = ASTUnit }

evaluateEnv :: Depth -> [AST] -> LContext AST
evaluateEnv d asts = do
    let envAst = head asts
    when (d > 1) $ throwL (astPos envAst) $ "env! can only be called on the top level"

    env <- getEnv
    let pairs = M.assocs env
    let longestKey = L.maximumBy (compare `on` (length . fst)) pairs $> fst
    let pad s = s ++ take (length longestKey + 4 - length s) (L.repeat ' ')
    let rows = pairs $> map (\(k, v) -> pad k ++ show v)
    liftIO $ mapM_ putStrLn rows
    return $ envAst { astNode = ASTUnit }

evaluateImport :: Depth -> [AST] -> LContext AST
evaluateImport d asts = do
    let importAst = head asts
        args = tail asts
    when (d > 1) $ throwL (astPos importAst) $ "import! can only be called on the top level"

    case args of
        [AST { astNode = ASTSymbol qualifier }, AST { astNode = ASTString path }] -> do
            builtinState <- getBuiltinState
            LState { stateEnv = evaledRawEnv } <- lift $ execStateT (runScriptFile path) builtinState
            let exportsVecASTM = M.lookup "exports" evaledRawEnv
            exportedEnv <- case exportsVecASTM of
                Just (AST { astNode = ASTVector exportsVec }) -> do
                    exportSyms <- exportsVec $>
                        mapM (\case AST { astNode = ASTSymbol sym } -> return sym
                                    ast -> throwL (astPos ast) $ "non-symbol value in exports vector: " ++ show ast)
                    let resultEnv = M.filterWithKey (\k _ -> L.elem k exportSyms) evaledRawEnv
                    return resultEnv
                Just ast -> throwL (astPos ast) $ "exports symbol set to non-symbol value: " ++ show ast
                Nothing -> throwL (astPos importAst) $ "no exports vector defined in file: " ++ path

            let nameMangled = M.mapKeys (\k -> qualifier ++ ":" ++ k) exportedEnv
            env <- getEnv
            putEnv $ M.union env nameMangled
            return $ importAst { astNode = ASTUnit }
        [AST { astNode = ASTString path }] -> do
            builtinState <- getBuiltinState
            LState { stateEnv = evaledRawEnv } <- lift $ execStateT (runScriptFile path) builtinState
            let exportsVecASTM = M.lookup "exports" evaledRawEnv
            exportedEnv <- case exportsVecASTM of
                Just (AST { astNode = ASTVector exportsVec }) -> do
                    exportSyms <- exportsVec $>
                        mapM (\case AST { astNode = ASTSymbol sym } -> return sym
                                    ast -> throwL (astPos ast) $ "non-symbol value in exports vector: " ++ show ast)
                    let resultEnv = M.filterWithKey (\k _ -> L.elem k exportSyms) evaledRawEnv
                    return resultEnv
                Just ast -> throwL (astPos ast) $ "exports symbol set to non-symbol value: " ++ show ast
                Nothing -> throwL (astPos importAst) $ "no exports vector defined in file: " ++ path

            env <- getEnv
            putEnv $ M.union env exportedEnv
            return $ importAst { astNode = ASTUnit }

        _ -> throwL (astPos importAst) $ "invalid arguments passed to import!: " ++ show args

evaluateUserFunction :: Depth -> [AST] -> LContext AST
evaluateUserFunction d children = do
    let fnAst = head children
        args = tail children
    fnEvaled <- evaluate d fnAst
    AST { astNode = (ASTFunction fn) } <- assertIsASTFunction fnEvaled
    evaledArgs <- mapM (evaluate d) args
    doubleEvaledArgs <- mapM (evaluate d) evaledArgs

    result <- curryCall (reverse doubleEvaledArgs) fn
    -- todo: maybe remove double eval here? can't remember why it was added
    return $ fnAst { astNode = astNode result }

evaluateSymbol :: AST -> LContext AST
evaluateSymbol ast@AST { astNode = ASTSymbol sym }  = do
    env <- getEnv
    let val = M.lookup sym env
    case val of
        Just ast' -> return ast'
        Nothing -> throwL (astPos ast) $ "symbol " ++ sym ++ " not defined in environment"
evaluateSymbol ast = throwL (astPos ast) $ "unreachable: evaluateSymbol, ast: " ++ show ast

evaluate :: Depth -> AST -> LContext AST
evaluate d AST { astNode = fnc@(ASTFunctionCall args@(x:_)) } =
     do config <- getConfig
        when (configPrintCallStack config) $ liftIO $ putStrLn $ "fn call: " ++ show fnc
        case astNode x of
            -- remember to add these as reseved keywords in Builtins!
            ASTSymbol "\\" ->
                evaluateFunctionDef (d + 1) args
            ASTSymbol "match" ->
                evaluateMatch (d + 1) args
            ASTSymbol "let!" ->
                evaluateLet (d + 1) args
            ASTSymbol "env!" ->
                evaluateEnv (d + 1) args
            ASTSymbol "import!" ->
                evaluateImport (d + 1) args
            _ ->
                evaluateUserFunction (d + 1) args
evaluate _ ast@AST { astNode = (ASTSymbol _) } =
    evaluateSymbol ast
evaluate d ast@AST { astNode = (ASTVector vec) } =
     do rets <- mapM (evaluate (d + 1)) vec
        return $ ast { astNode = ASTVector rets }
evaluate _ other =
    return other

-- LIB

runScriptFile :: String -> LContext [AST]
runScriptFile fileName = do
    src <- liftIO $ readFile fileName
    runInlineScript fileName src

getBuiltinState :: LContext LState
getBuiltinState = do
    config <- getConfig
    return $ LState { stateConfig = config, stateEnv = builtinEnv }

runInlineScript :: String -> String -> LContext [AST]
runInlineScript fileName src = do
    tokenized <- tokenize fileName src
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
                newAst <- evaluate 0 ast
                restEvaled <- foldEvaluate rest
                return $ newAst : restEvaled
