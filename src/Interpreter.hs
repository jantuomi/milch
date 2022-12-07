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
import qualified Data.Bifunctor as B
import Data.Function ( on )
import Control.Monad.State
import Control.Monad.Except
import Control.Exception ( try )
import Utils
import Builtins
import Tokenizer ( tokenize' )
import Parser ( parse )

curryCall :: [AST] -> ASTNode -> LContext AST
curryCall [] (ASTFunction fIsPure f) = do
    checkPurity fIsPure
    f (makeNonsenseAST ASTUnit)
curryCall (arg:[]) (ASTFunction fIsPure f) = do
    checkPurity fIsPure
    f arg
curryCall (arg:rest) f = do
    g <- curryCall rest f
    case an g of
        ASTFunction fIsPure f' -> do
            checkPurity fIsPure
            f' arg
        other -> throwL (astPos g) $ "cannot call value " ++ show other ++ " as a function"
curryCall _ astFn = throwL "" $ "unreachable: curryCall, astFn: " ++ show astFn

traverseAndReplace :: String -> AST -> AST -> AST
traverseAndReplace param arg ast@AST { an = ASTSymbol sym }
    | sym == param = arg
    | otherwise = ast
traverseAndReplace param arg ast@AST { an = ASTFunctionCall body } =
    ast { an = ASTFunctionCall $ (map (traverseAndReplace param arg) body) }
traverseAndReplace param arg ast@AST { an = ASTVector vec } =
    ast { an = ASTVector $ (map (traverseAndReplace param arg) vec) }
traverseAndReplace param arg ast@AST { an = ASTHashMap hmap } =
    ast { an = ASTHashMap $ M.assocs hmap
        $> L.concatMap (\(a, b) -> [a, b])
        .> map (traverseAndReplace param arg)
        .> asPairs .> M.fromList }
traverseAndReplace _ _ other = other

foldScope :: Scope -> AST -> AST
foldScope [] body = body
foldScope ((sym, val):rest) body =
    let replacedRest = map (B.second $ traverseAndReplace sym val) rest
        replacedBody = traverseAndReplace sym val body
     in foldScope replacedRest replacedBody

processLetExpr :: Scope -> AST -> LContext Scope
processLetExpr scope letExpr = do
    (sym, val) <- case letExpr of
            AST { an = ASTFunctionCall [AST { an = ASTSymbol "let" }, AST { an = ASTSymbol symbol' }, value'] } ->
                return (symbol', value')
            other -> throwL (astPos other) $ "invalid let call in function body: " ++ show other

    return $ (sym, val) : scope

foldUserFunctionLetExprs :: Scope -> AST -> [AST] -> LContext LFunction
foldUserFunctionLetExprs scope paramAst@AST { an = ASTSymbol param } exprs = return fn where
    fn :: LFunction
    fn arg = ret `catchError` appendError ("in a function definition at " ++ astPos paramAst) where
        ret = do
            let letExprs = init exprs

            let localScopeWithArgs = (param, arg) : scope
            localScope <- foldM processLetExpr localScopeWithArgs letExprs

            let body = last exprs
            let newBody = foldScope localScope body
            evaluate newBody

foldUserFunctionLetExprs _ param exprs = throwL (astPos param)
    $ "unreachable: foldUserFunctionLetExprs, param: " ++ show param ++ ", exprs: " ++ show exprs

foldUserFunctionParams :: Scope -> [AST] -> [AST] -> LContext LFunction
foldUserFunctionParams scope [] exprs =
    -- the position info is nonsensical, but it should never get read anyway
    foldUserFunctionLetExprs scope (makeNonsenseAST $ ASTSymbol "unit") exprs
foldUserFunctionParams scope (param:[]) exprs =
    foldUserFunctionLetExprs scope param exprs
foldUserFunctionParams scope (AST { an = ASTSymbol param }:rest) exprs = return fn where
    fn :: LFunction
    fn arg = do
        let scopeWithCurrentArg = (param, arg) : scope
        ret <- foldUserFunctionParams scopeWithCurrentArg rest exprs
        -- The returned function AST will not have the correct position info or purity, but that's fine
        -- because the info is overridden in evaluateFunctionDef anyway.
        return $ makeNonsenseAST $ ASTFunction Pure ret
foldUserFunctionParams _ (param:_) _ = throwL (astPos $ param)
    $ "unreachable: foldUserFunctionParams, param: " ++ show param

defineUserFunction :: [AST] -> [AST] -> LContext LFunction
defineUserFunction = foldUserFunctionParams []

evaluateFunctionDef :: Purity -> [AST] -> LContext AST
evaluateFunctionDef isPure asts = do
    let defAst = head asts
        args = tail asts
    (params'', exprs) <- case args of
        args'
            | length args' < 2 ->
                throwL (astPos defAst) $ "\\ or \\! called with " ++ show (length args) ++ " arguments"
            | otherwise -> return $ (head args', tail args')

    AST { an = ASTVector params' } <- assertIsASTVector params''
    params <- mapM assertIsASTSymbol params'

    env <- getEnv
    let isParamNameShadowing name = MB.isJust $ resolveSymbol name env

    let shadowingParamM = L.find (asSymbol .> isParamNameShadowing) params
    case shadowingParamM of
        Just shadowingParam -> throwL (astPos shadowingParam)
            $ "parameter is shadowing already defined symbol " ++ show (an shadowingParam)
        Nothing -> return ()

    let letExprs = init exprs
    let nonLetExprM = L.find (not . isLetAST) letExprs
    case nonLetExprM of
        Just nonLetExpr -> throwL (astPos nonLetExpr)
            $ "non-let expression in function definition before body: " ++ show nonLetExpr
        Nothing -> return ()

    fn <- defineUserFunction params exprs
    return $ defAst { an = ASTFunction isPure fn }
    where
        isLetAST AST { an = ASTFunctionCall (AST { an = ASTSymbol "let" }:_) } = True
        isLetAST _ = False
        asSymbol AST { an = ASTSymbol sym } = sym
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
    return $ matchAst { an = an ret }
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
    when (d > 1) $ throwL (astPos letAst) $ "let can only be called on the top level or in a function definition, current depth: " ++ show d

    (symbol, value) <- case args of
        [AST { an = ASTSymbol "lazy" }, AST { an = ASTSymbol symbol' }, value'] -> do
            return (symbol', value')
        [AST { an = ASTSymbol symbol' }, value'] -> do
            evaledValue <- evaluate value'
            return (symbol', evaledValue)
        other -> throwL (astPos $ head other) $ "let called with invalid args " ++ show other

    env <- getEnv
    when (MB.isJust $ resolveSymbol symbol env) $ throwL (astPos letAst) $ "symbol already defined: " ++ symbol
    insertEnv symbol value
    return $ letAst { an = ASTUnit }

evaluateDebugEnv :: [AST] -> LContext AST
evaluateDebugEnv asts = do
    let envAst = head asts

    env <- getEnv
    let pairs = M.assocs env
    let longestKey = L.maximumBy (compare `on` (length . fst)) pairs $> fst
    let pad s = s ++ take (length longestKey + 4 - length s) (L.repeat ' ')
    let rows = pairs $> map (\(k, v) -> pad k ++ show v)
    liftIO $ mapM_ putStrLn rows
    return $ envAst { an = ASTUnit }

evaluateImport :: [AST] -> LContext AST
evaluateImport asts = do
    let importAst = head asts
        args = tail asts

    d <- getDepth
    when (d > 1) $ throwL (astPos importAst) $
        "import can only be called on the top level, current depth: " ++ show d

    config <- getConfig
    let initialState = LState {
        stateConfig = config,
        stateDepth = 0,
        stateEnv = builtinEnv,
        statePure = Impure
    }
    case args of
        -- non-qualified import
        [AST { an = ASTString path }] -> do
            let checkedPath = if (not $ ".milch" `L.isSuffixOf` path)
                then (path ++ ".milch")
                else path
            LState { stateEnv = importedEnv } <- lift $ execStateT (runScriptFile checkedPath) initialState
            env <- getEnv

            putEnv $ M.union importedEnv env
            return $ importAst { an = ASTUnit }

        _ -> throwL (astPos importAst) $ "invalid arguments passed to import: " ++ show args

evaluateRecord :: [AST] -> LContext AST
evaluateRecord asts = do
    let recordAst = head asts
        args = tail asts

    d <- getDepth
    when (d > 1) $ throwL (astPos recordAst) $ "record can only be called on the top level, current depth: " ++ show d

    case args of
        (AST { an = ASTSymbol ns }:rest) -> do
            let nonSymFields = L.find (not . isSymbolAST) rest
            case nonSymFields of
                Just invalid -> throwL (astPos invalid)
                    $ "non-symbol field in record definition: " ++ show invalid
                Nothing -> return ()

            let fields = extractRows rest

            let fnCreateName = ns ++ "/create"
            let makeFnCreate :: [String] -> [(String, AST)] -> LFunction
                makeFnCreate (param:[]) argsAcc = fn where
                    fn arg = do
                        let record = M.fromList $ (param, arg) : argsAcc
                        return $ makeNonsenseAST $ ASTRecord ns record
                makeFnCreate (param:restParams) argsAcc = fn where
                    fn :: LFunction
                    fn arg = return $ makeNonsenseAST $ ASTFunction Pure $
                        makeFnCreate restParams ((param, arg):argsAcc)

                makeFnCreate _ _ = error $ "unreachable: makeFnCreate " ++ fnCreateName

            let createFn = makeFnCreate fields []
            insertEnv fnCreateName $ recordAst { an = ASTFunction Pure createFn }

            let makeGetFns [] = return $ ()
                makeGetFns (param:restParams) = do
                    let fnGetName = ns ++ "/" ++ "get-" ++ param
                    let fn = getFn fnGetName
                    let fnAST = makeNonsenseAST $ ASTFunction Pure $ fn
                    insertEnv fnGetName fnAST
                    makeGetFns restParams

            makeGetFns fields

            let makeSetFns [] = return $ ()
                makeSetFns (param:restParams) = do
                    let fnSetName = ns ++ "/" ++ "set-" ++ param
                    let fn = setFn fnSetName
                    let fnAST = makeNonsenseAST $ ASTFunction Pure $ fn
                    insertEnv fnSetName fnAST
                    makeSetFns restParams

            makeSetFns fields

            return $ recordAst { an = ASTUnit }

        _ -> throwL (astPos recordAst) $ "invalid arguments passed to import: " ++ show args

    where
        isSymbolAST AST { an = ASTSymbol _ } = True
        isSymbolAST _ = False
        extractRows (AST { an = ASTSymbol sym }:rest) = sym : extractRows rest
        extractRows _ = []
        getFn fnName ast@AST { an = ASTRecord identifier record } = do
            when (not $ identifier `L.isPrefixOf` fnName) $
                throwL (astPos ast) $ "invalid argument: " ++ fnName ++ " cannot operate on record " ++ identifier
            let (_, fnId) = separateNsIdPart fnName
            let fieldId = drop 4 fnId
            case (M.lookup fieldId record) of
                Just value -> return $ value
                Nothing -> error $ "unreachable: getFn " ++ fnName
        getFn fnName ast = throwL (astPos ast) $ "invalid argument passed to " ++ fnName ++ ": " ++ (show ast)
        setFn fnName ast1 = do
            return $ makeNonsenseAST $ ASTFunction Pure $ fn where
                fn ast2@AST { an = ASTRecord identifier record } = do
                    when (not $ identifier `L.isPrefixOf` fnName) $
                        throwL (astPos ast2) $ "invalid argument: " ++ fnName ++ " cannot operate on record " ++ identifier
                    let (_, fnId) = separateNsIdPart fnName
                    let fieldId = drop 4 fnId
                    let newRecord = M.insert fieldId ast1 record
                    return $ makeNonsenseAST $ ASTRecord identifier newRecord
                fn ast2 = throwL (astPos ast2) $ "invalid argument passed to " ++ fnName ++ ": " ++ (show ast2)

evaluateUserFunction :: [AST] -> LContext AST
evaluateUserFunction children = do
    let fnAst = head children
        args = tail children
    fnEvaled <- evaluate fnAst
    AST { an = astFn@(ASTFunction fIsPure _) } <- assertIsASTFunction fnEvaled

    checkPurity fIsPure
    updatePurity fIsPure

    evaledArgs <- mapM evaluate args
    doubleEvaledArgs <- mapM evaluate evaledArgs

    result <- curryCall (reverse doubleEvaledArgs) astFn

    -- todo: maybe remove double eval here? can't remember why it was added
    return $ fnAst { an = an result }

resolveSymbol :: String -> Env -> Maybe AST
resolveSymbol = M.lookup

evaluateSymbol :: AST -> LContext AST
evaluateSymbol ast@AST { an = ASTSymbol sym } = do
    env <- getEnv
    let val = resolveSymbol sym env
    case val of
        Just ast' -> return ast'
        Nothing -> throwL (astPos ast) $ "symbol " ++ sym ++ " not defined in environment"
evaluateSymbol ast = throwL (astPos ast) $ "unreachable: evaluateSymbol, ast: " ++ show ast

evaluate :: AST -> LContext AST
evaluate ast@AST { an = fnc@(ASTFunctionCall args@(x:_)) } =
     do config <- getConfig
        when (configPrintCallStack config) $ liftIO $ putStrLn $ "fn call: " ++ show fnc
        incrementDepth

        currentPurity <- getPurity

        let task = case an x of
            -- remember to add these as reseved keywords in Builtins!
                ASTSymbol "\\" ->
                    evaluateFunctionDef Pure args
                ASTSymbol "\\!" ->
                    evaluateFunctionDef Impure args
                ASTSymbol "match" ->
                    evaluateMatch args
                ASTSymbol "let" ->
                    evaluateLet args
                ASTSymbol "Debug/env" ->
                    evaluateDebugEnv args
                ASTSymbol "import" ->
                    evaluateImport args
                ASTSymbol "record" ->
                    evaluateRecord args
                _ ->
                    evaluateUserFunction args

        ret <- task `catchError`
            appendError ("when calling function " ++ show x ++ " at " ++ astPos ast)

        decrementDepth
        updatePurity currentPurity

        return ret
evaluate ast@AST { an = (ASTSymbol _) } =
    evaluateSymbol ast
evaluate ast@AST { an = (ASTVector vec) } =
     do incrementDepth
        rets <- mapM evaluate vec `catchError`
                    appendError ("when evaluating elements of vector " ++ show vec ++ " at " ++ astPos ast)
        decrementDepth
        return $ ast { an = ASTVector rets }
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

    case configPrintEvaled config of
        PrintEvaledAll -> liftIO $ mapM_ putStrLn (map show evaluated)
        PrintEvaledNonUnit -> do
            let evaledNonUnits = evaluated $>
                    filter (\case AST { an = ASTUnit } -> False
                                  _ -> True)
            liftIO $ mapM_ putStrLn (map show evaledNonUnits)
        PrintEvaledOff -> return ()

    return evaluated
        where
            foldEvaluate :: [AST] -> LContext [AST]
            foldEvaluate [] = return []
            foldEvaluate (ast:rest) = do
                newAst <- evaluate ast
                restEvaled <- foldEvaluate rest
                return $ newAst : restEvaled