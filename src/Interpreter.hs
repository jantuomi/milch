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
            other -> throwL (astPos other, "invalid let call in function body: " ++ show other)

    return $ (sym, val) : scope

foldUserFunctionLetExprs :: Purity -> Scope -> AST -> [AST] -> LContext LFunction
foldUserFunctionLetExprs callingCtxPurity scope paramAst@AST { an = ASTSymbol param } exprs = return fn where
    fn :: LFunction
    fn arg = ret `catchError` appendError (astPos paramAst, "in a function definition") where
        ret = do
            let letExprs = init exprs

            let localScopeWithArgs = (param, arg) : scope
            localScope <- foldM processLetExpr localScopeWithArgs letExprs

            let body = last exprs
            let newBody = foldScope localScope body

            updatePurity callingCtxPurity
            evaluate newBody

foldUserFunctionLetExprs _ _ param exprs = throwL (astPos param,
    "unreachable: foldUserFunctionLetExprs, param: " ++ show param ++ ", exprs: " ++ show exprs)

foldUserFunctionParams :: Purity -> Scope -> [AST] -> [AST] -> LContext LFunction
foldUserFunctionParams _ _ [] _ =
    throwL $ ("", "cannot define a function with zero parameters")
foldUserFunctionParams callingCtxPurity scope (param:[]) exprs =
    foldUserFunctionLetExprs callingCtxPurity scope param exprs
foldUserFunctionParams callingCtxPurity scope (AST { an = ASTSymbol param }:rest) exprs = return fn where
    fn :: LFunction
    fn arg = do
        let scopeWithCurrentArg = (param, arg) : scope
        ret <- foldUserFunctionParams callingCtxPurity scopeWithCurrentArg rest exprs
        -- The returned function AST will not have the correct position info or purity, but that's fine
        -- because the info is overridden in evaluateFunctionDef anyway.
        return $ makeNonsenseAST $ ASTFunction Pure ret
foldUserFunctionParams _ _ (param:_) _ = throwL (astPos $ param,
    "unreachable: foldUserFunctionParams, param: " ++ show param)

defineUserFunction :: Purity -> [AST] -> [AST] -> LContext LFunction
defineUserFunction callingCtxPurity = foldUserFunctionParams callingCtxPurity []

evaluateFunctionDef :: Purity -> [AST] -> LContext AST
evaluateFunctionDef fPurity asts = do
    let defAst = head asts
        args = tail asts
    (params'', exprs) <- case args of
        args'
            | length args' < 2 ->
                throwL (astPos defAst, "\\ or \\! called with " ++ show (length args) ++ " arguments")
            | otherwise -> return $ (head args', tail args')

    params' <- case params'' of
        AST { an = ASTVector vec } -> return vec
        -- vector literals like [a b c] are turned into (eval [a b c]) in the parser, and this eval call
        -- must be unwrapped. this is not the cleanest way to do this but this way we can avoid having special
        -- evaluation logic for collection types.
        AST { an = ASTFunctionCall ([ AST { an = ASTSymbol "eval" }, AST { an = ASTVector vec }])}
            -> return vec
        other -> throwL (astPos params'',
                         "non-vector value used as parameter list in function definition: " ++ show other)

    params <- mapM assertIsASTSymbol params'

    env <- getEnv
    let isParamNameShadowing name = MB.isJust $ resolveSymbol name env

    let shadowingParamM = L.find (asSymbol .> isParamNameShadowing) params
    case shadowingParamM of
        Just shadowingParam -> throwL (astPos shadowingParam,
            "parameter is shadowing already defined symbol " ++ show (an shadowingParam))
        Nothing -> return ()

    let letExprs = init exprs
    let nonLetExprM = L.find (not . isLetAST) letExprs
    case nonLetExprM of
        Just nonLetExpr -> throwL (astPos nonLetExpr,
            "non-let expression in function definition before body: " ++ show nonLetExpr)
        Nothing -> return ()

    fn <- defineUserFunction fPurity params exprs
    return $ defAst { an = ASTFunction fPurity fn }
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
        [] -> throwL (astPos matchAst, "match called with no arguments")
        (_:[]) -> throwL (astPos matchAst, "empty match cases")
        (a:b) -> return (a, b)

    pairs <- (asPairsM rest) `catchError`
                appendError (astPos matchAst, "invalid number of arguments passed to match\n"
                                ++ "- matching on expr: " ++ show actualExpr ++ "\n"
                                ++ "- arguments: " ++ show rest)

    evaledActual <- evaluate actualExpr
    ret <- matchPairs (actualExpr, evaledActual) pairs
    return $ matchAst { an = an ret }
    where
        matchPairs :: (AST, AST) -> [(AST, AST)] -> LContext AST
        matchPairs (actualExpr, evaledActual) [] = throwL (astPos actualExpr,
            "matching case not found when matching on expression: " ++ show actualExpr
            ++ " (evaled value: " ++ show evaledActual ++ ")")
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
    when (d > 1) $ throwL (astPos letAst, "let can only be called on the top level or in a function definition, current depth: " ++ show d)

    case args of
        [AST { an = ASTSymbol "lazy" }, AST { an = ASTSymbol sym }, val] -> do
            env <- getEnv
            when (MB.isJust $ resolveSymbol sym env) $ throwL (astPos letAst, "symbol already defined: " ++ sym)
            insertEnv sym $ Regular val
            return $ letAst { an = ASTUnit }
        [AST { an = ASTSymbol "memo" }, AST { an = ASTSymbol sym }, val] -> do
            env <- getEnv
            when (MB.isJust $ resolveSymbol sym env) $ throwL (astPos letAst, "symbol already defined: " ++ sym)
            insertEnv sym $ Memoized M.empty val
            return $ letAst { an = ASTUnit }
        [AST { an = ASTSymbol sym }, value'] -> do
            evaledValue <- evaluate value'
            env <- getEnv
            when (MB.isJust $ resolveSymbol sym env) $ throwL (astPos letAst, "symbol already defined: " ++ sym)
            insertEnv sym $ Regular evaledValue
            return $ letAst { an = ASTUnit }
        other -> throwL (astPos $ head other, "let called with invalid args " ++ show other)

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
    when (d > 1) $ throwL (astPos importAst, "import can only be called on the top level, current depth: " ++ show d)

    config <- getConfig
    atomMap <- getAtomMap
    env <- getEnv

    path <- case args of
        [AST { an = ASTString path }] -> return path
        other -> throwL (astPos importAst, "invalid args passed to import: " ++ show other)

    let checkedPath = if (not $ ".milch" `L.isSuffixOf` path)
        then (path ++ ".milch")
        else path

    let initialState = LState {
        stateConfig = config { configScriptFileName = Just checkedPath },
        stateDepth = 0,
        stateEnv = builtinEnv,
        statePure = Impure,
        stateAtomMap = atomMap
    }

    LState { stateEnv = importedEnv, stateAtomMap = importedAtomMap }
        <- lift $ execStateT (runScriptFile checkedPath) initialState

    let conflicting = M.intersection importedEnv env
            $> M.assocs
            .> map (\(k, (importedSm, _)) -> (k, importedSm, fst $ env M.! k))
            .> filter (\(_, sm1, sm2) -> sm1 /= sm2)

    when (length conflicting > 0) $
        throwL (astPos importAst, "import shadows symbols: "
            ++ L.intercalate ", " (map (\(k, _, sm) -> k ++ " (defined in " ++ show sm ++ ")") conflicting))

    putEnv $ M.union importedEnv env
    putAtomMap $ M.union importedAtomMap atomMap

    return $ importAst { an = ASTUnit }

evaluateRecord :: [AST] -> LContext AST
evaluateRecord asts = do
    let recordAst = head asts
        args = tail asts

    d <- getDepth
    when (d > 1) $ throwL (astPos recordAst, "record can only be called on the top level, current depth: " ++ show d)

    case args of
        (AST { an = ASTSymbol ns }:rest) -> do
            let nonSymFields = L.find (not . isSymbolAST) rest
            case nonSymFields of
                Just invalid -> throwL (astPos invalid, "non-symbol field in record definition: " ++ show invalid)
                Nothing -> return ()

            let fields = extractRows rest

            let fnCreateName = ns ++ "/create"
            let makeFnCreate :: [String] -> [(String, AST)] -> LFunction
                makeFnCreate (param:[]) argsAcc = fn where
                    fn arg = do
                        let record = M.fromList $ (param, arg) : argsAcc
                        return $ makeNonsenseAST $ ASTRecord (computeTagN ns) ns record
                makeFnCreate (param:restParams) argsAcc = fn where
                    fn :: LFunction
                    fn arg = return $ makeNonsenseAST $ ASTFunction Pure $
                        makeFnCreate restParams ((param, arg):argsAcc)

                makeFnCreate _ _ = error $ "unreachable: makeFnCreate " ++ fnCreateName

            let createFn = makeFnCreate fields []
            insertEnv fnCreateName $ Regular $ recordAst { an = ASTFunction Pure createFn }

            let makeGetFns [] = return $ ()
                makeGetFns (param:restParams) = do
                    let fnGetName = ns ++ "/" ++ "get-" ++ param
                    let fn = getFn fnGetName
                    let fnAST = makeNonsenseAST $ ASTFunction Pure $ fn
                    insertEnv fnGetName $ Regular fnAST
                    makeGetFns restParams

            makeGetFns fields

            let makeSetFns [] = return $ ()
                makeSetFns (param:restParams) = do
                    let fnSetName = ns ++ "/" ++ "set-" ++ param
                    let fn = setFn fnSetName
                    let fnAST = makeNonsenseAST $ ASTFunction Pure $ fn
                    insertEnv fnSetName $ Regular fnAST
                    makeSetFns restParams

            makeSetFns fields

            return $ recordAst { an = ASTUnit }

        _ -> throwL (astPos recordAst, "invalid arguments passed to import: " ++ show args)

    where
        isSymbolAST AST { an = ASTSymbol _ } = True
        isSymbolAST _ = False
        extractRows (AST { an = ASTSymbol sym }:rest) = sym : extractRows rest
        extractRows _ = []
        getFn fnName ast@AST { an = ASTRecord _ identifier record } = do
            when (not $ identifier `L.isPrefixOf` fnName) $
                throwL (astPos ast, "invalid argument: " ++ fnName ++ " cannot operate on record " ++ identifier)
            let (_, fnId) = separateNsIdPart fnName
            let fieldId = drop 4 fnId
            case (M.lookup fieldId record) of
                Just value -> return $ value
                Nothing -> error $ "unreachable: getFn " ++ fnName
        getFn fnName ast = throwL (astPos ast, "invalid argument passed to " ++ fnName ++ ": " ++ (show ast))
        setFn fnName ast1 = do
            return $ makeNonsenseAST $ ASTFunction Pure $ fn where
                fn ast2@AST { an = ASTRecord tagHash identifier record } = do
                    when (not $ identifier `L.isPrefixOf` fnName) $
                        throwL (astPos ast2, "invalid argument: " ++ fnName ++ " cannot operate on record " ++ identifier)
                    let (_, fnId) = separateNsIdPart fnName
                    let fieldId = drop 4 fnId
                    let newRecord = M.insert fieldId ast1 record
                    return $ makeNonsenseAST $ ASTRecord tagHash identifier newRecord
                fn ast2 = throwL (astPos ast2, "invalid argument passed to " ++ fnName ++ ": " ++ (show ast2))

data ReifyResult
    = ReifyRegularFunction AST
    | ReifyMemoizedFunction String AST

reifyFunctionReference :: AST -> LContext ReifyResult
reifyFunctionReference ref = case ref of
    AST { an = ASTSymbol sym } -> do
        env <- getEnv
        let bindingM = resolveSymbol sym env
        case bindingM of
            Just (_, binding) -> case binding of
                Regular bound -> return $ ReifyRegularFunction $ bound
                Memoized _memoMap bound -> return $ ReifyMemoizedFunction sym $ bound
            Nothing -> throwL (astPos ref, "symbol " ++ sym ++ " not defined in environment")
    other -> do
        ret <- evaluate other
        return $ ReifyRegularFunction ret

unsafeGetMemoMap :: String -> LContext (M.Map [AST] AST)
unsafeGetMemoMap sym = do
    env <- getEnv
    case ((M.!) env sym) of
        (_, Memoized memoMap _) -> return memoMap
        _ -> error $ "unreachable: unsafeGetMemoMap " ++ show env ++ ", " ++ sym

callFunction :: AST -> AST -> LContext AST
callFunction AST { an = ASTFunction fPurity f } argAst = do
    checkPurity fPurity
    evaledArg <- evaluate argAst
    f evaledArg
callFunction other _ = throwL (astPos other, "cannot call non-function value: " ++ show other)

evaluateFunctionCall :: [AST] -> LContext AST
evaluateFunctionCall children = do
    let fnAst = head children
        args = tail children

    reifyRes <- reifyFunctionReference fnAst

    case reifyRes of
        ReifyRegularFunction bound -> do
            evaledBound <- evaluate bound
            let fnCallComponents = evaledBound : args

            result <- fold1M callFunction fnCallComponents
            return $ fnAst { an = an result }
        ReifyMemoizedFunction sym bound -> do
            evaledArgs <- mapM evaluate args

            memoMap <- unsafeGetMemoMap sym

            case (M.lookup evaledArgs memoMap) of
                Just hit -> return hit
                Nothing -> do
                    evaledBound <- evaluate bound
                    let fnCallComponents = evaledBound : args

                    result <- fold1M callFunction fnCallComponents

                    possiblyUpdatedMemoMap <- unsafeGetMemoMap sym
                    let newMemoMap = M.insert evaledArgs result possiblyUpdatedMemoMap

                    insertEnv sym $ Memoized newMemoMap bound
                    return $ fnAst { an = an result }

resolveSymbol :: String -> Env -> Maybe (SourceModule, Binding AST)
resolveSymbol = M.lookup

evaluateDo :: [AST] -> LContext AST
evaluateDo children = do
    let fnAst = head children
        args = tail children

    when (length args == 0) $ throwL (astPos fnAst, "do called with 0 arguments")

    let nonRetExprs = init args
    let retExpr = last args

    mapM_ evaluate nonRetExprs

    evaluate retExpr

evaluateImmediateEval :: [AST] -> LContext AST
evaluateImmediateEval children = do
    let fnAst = head children
        args = tail children

    when (length args /= 1) $ throwL (astPos fnAst, "eval called with invalid number of arguments: " ++ show (length args))
    case head args of
        ast@AST { an = ASTVector vec } -> do
            rets <- mapM evaluate vec `catchError`
                            appendError (astPos ast, "when evaluating elements of vector")
            return $ ast { an = ASTVector rets }
        ast@AST { an = ASTHashMap hmap } -> do
            let pairs = M.assocs hmap
            evaledPairs <- mapM (\(k, v) -> do evaledK <- evaluate k
                                               evaledV <- evaluate v
                                               return (evaledK, evaledV)) pairs `catchError`
                            appendError (astPos ast, "when evaluating keys and values of hash map")
            return $ ast { an = ASTHashMap $ M.fromList evaledPairs }
        arg -> evaluate arg

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
                ASTSymbol "Debug/env!" ->
                    evaluateDebugEnv args
                ASTSymbol "import" ->
                    evaluateImport args
                ASTSymbol "record" ->
                    evaluateRecord args
                ASTSymbol "do" ->
                    evaluateDo args
                ASTSymbol "eval" ->
                    evaluateImmediateEval args
                _ ->
                    evaluateFunctionCall args

        ret <- task `catchError`
            appendError (astPos ast, "when calling function " ++ show x)

        decrementDepth
        updatePurity currentPurity

        return ret
evaluate ast@AST { an = ASTSymbol sym } = do
    env <- getEnv
    let bindingM = resolveSymbol sym env
    case bindingM of
        Just (_, binding) -> return $ case binding of
            Regular v -> v
            Memoized _ v -> v
        Nothing -> throwL (astPos ast, "symbol " ++ sym ++ " not defined in environment")
evaluate other =
    return other

-- LIB

runScriptFile :: String -> LContext [AST]
runScriptFile fileName = do
    let srcM :: IO (Either IOError String)
        srcM = case fileName of
            "-" -> try $ getLine
            _ -> try $ readFile fileName

    srcM' <- liftIO srcM
    src <- case srcM' of
        Right s -> return s
        Left _ -> throwL ("", "Failed to open file: \"" ++ fileName ++ "\"")
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

    return evaluated
        where
            foldEvaluate :: [AST] -> LContext [AST]
            foldEvaluate [] = return []
            foldEvaluate (ast:rest) = do
                config <- getConfig
                evaledAst <- evaluate ast

                liftIO $ case configPrintEvaled config of
                    PrintEvaledAll -> putStrLn $ show evaledAst
                    PrintEvaledNonUnit -> case evaledAst of
                        AST { an = ASTUnit } -> return ()
                        _ -> putStrLn $ show evaledAst
                    PrintEvaledOff -> return ()

                restEvaled <- foldEvaluate rest
                return $ evaledAst : restEvaled