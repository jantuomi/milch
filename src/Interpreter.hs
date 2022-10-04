{-# LANGUAGE LambdaCase #-}
module Interpreter (
    runInlineScript,
    runScriptFile
) where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Function ( on )
import Control.Monad.Reader
import Control.Monad.Except
import Utils
-- import Debug.Trace
import Builtins
import Tokenizer ( tokenize )
import Parser ( parse )

type Depth = Int

_curryCall :: Env -> [AST] -> LFunction -> LContext AST
_curryCall _ [] f = return $ (makeNonsenseAST $ ASTFunction f)
_curryCall env (arg:[]) f = f env arg
_curryCall env (arg:rest) f = do
    g <- _curryCall env rest f
    case astNode g of
        ASTFunction f' -> f' env arg
        other -> throwL (astPos g) $ "cannot call value " ++ show other ++ " as a function"

curryCall :: Env -> [AST] -> LFunction -> LContext AST
curryCall env [] f = f env (makeNonsenseAST ASTUnit)
curryCall env args f = _curryCall env args f

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

letArgsToSymValPairs :: Depth -> Env -> [AST] -> LContext (String, AST)
letArgsToSymValPairs d env args =
    case args of
        [AST { astNode = ASTSymbol symbol' }, value'] -> do
            (_, evaledValue) <- evaluate d env value'
            return (symbol', evaledValue)
        [AST { astNode = ASTSymbol "lazy" }, AST { astNode = ASTSymbol symbol' }, value'] -> do
            return (symbol', value')
        other -> throwL (astPos $ head other) $ "let! called with invalid args " ++ show other

defineUserFunction :: Depth -> AST -> [AST] -> LContext LFunction
defineUserFunction d AST { astNode = ASTSymbol param } exprs = return fn where
    fn :: LFunction
    fn env arg = do
        let replacedExprs = map (traverseAndReplace param arg) exprs
        let letExprs = take (length exprs - 1) replacedExprs
        letSymValPairs <- letExprs
                $> mapM (\case AST { astNode = ASTFunctionCall v } -> return $ drop 1 v
                               ast -> throwL (astPos ast) $ "unreachable: map letExprs, ast: " ++ show ast)
                .> fmap (mapM $ letArgsToSymValPairs d env) .> join
        let body = head $ drop (length exprs - 1) replacedExprs
        let newBody = traverseAndReplace param arg body
                $> foldSymValPairs letSymValPairs
        (_, ret) <- evaluate d env newBody
        return ret

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
    fn _ arg = do
        let newExprs = map (traverseAndReplace param arg) exprs
        ret <- defineUserFunctionWithLetExprs d rest newExprs
        -- the returned AST will not have the correct position info, but that's fine
        -- because the info is overridden in evaluateFunctionDef anyway
        return $ makeNonsenseAST $ ASTFunction $ ret
defineUserFunctionWithLetExprs _ (param:_) _ = throwL (astPos $ param)
    $ "unreachable: defineUserFunctionWithLetExprs, param: " ++ show param

evaluateFunctionDef :: Depth -> Env -> [AST] -> LContext (Env, AST)
evaluateFunctionDef d env asts = do
    let defAst = head asts
        args = tail asts
    (params'', exprs) <- case args of
        args'
            | length args' < 2 ->
                throwL (astPos defAst) $ "\\ called with " ++ show (length args) ++ " arguments"
            | otherwise -> return $ (head args', tail args')

    AST { astNode = ASTVector params' } <- assertIsASTVector params''
    params <- mapM assertIsASTSymbol params'

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
    return $ (env, defAst { astNode = ASTFunction fn })
    where
        isLetAST AST { astNode = ASTFunctionCall (AST { astNode = ASTSymbol "let!" }:_) } = True
        isLetAST _ = False
        isParamNameShadowing name = M.member name env

evaluateMatch :: Depth -> Env -> [AST] -> LContext (Env, AST)
evaluateMatch d env asts = do
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

    (_, evaledActual) <- evaluate d env actualExpr
    ret <- matchPairs (actualExpr, evaledActual) pairs
    return (env, matchAst { astNode = astNode ret })
    where
        matchPairs :: (AST, AST) -> [(AST, AST)] -> LContext AST
        matchPairs (actualExpr, evaledActual) [] = throwL (astPos actualExpr)
            $ "matching case not found when matching on expression: " ++ show actualExpr
            ++ " (actual value: " ++ show evaledActual ++ ")"
        matchPairs (actualExpr, evaledActual) ((matcher, branch):restPairs) = do
            (_, evaledMatcher) <- evaluate d env matcher
            if evaledActual == evaledMatcher
                then do
                    (_, ret) <- evaluate d env branch
                    return ret
                else matchPairs (actualExpr, evaledActual) restPairs

evaluateLet :: Depth -> Env -> [AST] -> LContext (Env, AST)
evaluateLet d env asts = do
    let letAst = head asts
        args = tail asts
    when (d > 1) $ throwL (astPos letAst) $ "let! can only be called on the top level or in a function definition"
    (symbol, value) <- letArgsToSymValPairs d env args
    when (M.member symbol env) $ throwL (astPos letAst) $ "symbol already defined: " ++ symbol
    let newEnv = M.insert symbol value env
    return $ (newEnv, letAst { astNode = ASTUnit })

evaluateEnv :: Depth -> Env -> [AST] -> LContext (Env, AST)
evaluateEnv d env asts = do
    let envAst = head asts
    when (d > 1) $ throwL (astPos envAst) $ "env! can only be called on the top level"
    let pairs = M.assocs env
    let longestKey = L.maximumBy (compare `on` (length . fst)) pairs $> fst
    let pad s = s ++ take (length longestKey + 4 - length s) (L.repeat ' ')
    let rows = pairs $> map (\(k, v) -> pad k ++ show v)
    liftIO $ mapM_ putStrLn rows
    return (env, envAst { astNode = ASTUnit })

evaluateImport :: Depth -> Env -> [AST] -> LContext (Env, AST)
evaluateImport d env asts = do
    let importAst = head asts
        args = tail asts
    when (d > 1) $ throwL (astPos importAst) $ "import! can only be called on the top level"

    case args of
        [AST { astNode = ASTSymbol qualifier }, AST { astNode = ASTString path }] -> do
            (importedEnv, _) <- runScriptFile builtinEnv path
            let nameMangled = M.mapKeys (\k -> qualifier ++ ":" ++ k) importedEnv
            return (M.union env nameMangled, importAst { astNode = ASTUnit })
        [AST { astNode = ASTString path }] -> do
            (importedEnv, _) <- runScriptFile builtinEnv path
            return (M.union env importedEnv, importAst { astNode = ASTUnit })
        _ -> throwL (astPos importAst) $ "invalid arguments passed to import!: " ++ show args

evaluateUserFunction :: Depth -> Env -> [AST] -> LContext (Env, AST)
evaluateUserFunction d env children = do
    let fnAst = head children
        args = tail children
    (_, fnEvaled) <- evaluate d env fnAst
    AST { astNode = (ASTFunction fn) } <- assertIsASTFunction fnEvaled
    evaledArgs' <- mapM (evaluate d env) args
    let evaledArgs = map snd evaledArgs'
    doubleEvaledArgs' <- mapM (evaluate d env) evaledArgs
    let doubleEvaledArgs = map snd doubleEvaledArgs'
    result <- curryCall env (reverse doubleEvaledArgs) fn
    -- maybe remove double eval here? can't remember why it was added
    return (env, fnAst { astNode = astNode result })

evaluateSymbol :: Env -> AST -> LContext (Env, AST)
evaluateSymbol env ast@AST { astNode = ASTSymbol sym }  = do
    let val = M.lookup sym env
    case val of
        Just ast' -> return (env, ast')
        Nothing -> throwL (astPos ast) $ "symbol " ++ sym ++ " not defined in environment"
evaluateSymbol _ ast = throwL (astPos ast) $ "unreachable: evaluateSymbol, ast: " ++ show ast

evaluate :: Depth -> Env -> AST -> LContext (Env, AST)
evaluate d env AST { astNode = fnc@(ASTFunctionCall args@(x:_)) } =
     do config <- ask
        when (configPrintCallStack config) $ liftIO $ putStrLn $ "fn call: " ++ show fnc
        case astNode x of
            -- remember to add these as reseved keywords in Builtins!
            ASTSymbol "\\" ->
                evaluateFunctionDef (d + 1) env args
            ASTSymbol "match" ->
                evaluateMatch (d + 1) env args
            ASTSymbol "let!" ->
                evaluateLet (d + 1) env args
            ASTSymbol "env!" ->
                evaluateEnv (d + 1) env args
            ASTSymbol "import!" ->
                evaluateImport (d + 1) env args
            _ ->
                evaluateUserFunction (d + 1) env args
evaluate _ env ast@AST { astNode = (ASTSymbol _) } =
    evaluateSymbol env ast
evaluate d env ast@AST { astNode = (ASTVector vec) } =
     do rets <- mapM (evaluate (d + 1) env) vec
        let vec' = map snd rets
        return $ (env, ast { astNode = ASTVector vec' })
evaluate _ env other =
    return (env, other)

-- LIB

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
