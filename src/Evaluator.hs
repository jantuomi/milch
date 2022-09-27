{-# LANGUAGE LambdaCase #-}
module Evaluator (
    evaluate,
) where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Function ( on )
import Control.Monad.Reader
import Control.Monad.Except ( catchError )
import Utils
-- import Debug.Trace

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

letArgsToSymValPairs :: Env -> [AST] -> LContext (String, AST)
letArgsToSymValPairs env args =
    case args of
        [AST { astNode = ASTSymbol symbol' }, value'] -> do
            (_, evaledValue) <- evaluate env value'
            return (symbol', evaledValue)
        [AST { astNode = ASTSymbol "lazy" }, AST { astNode = ASTSymbol symbol' }, value'] -> do
            return (symbol', value')
        other -> throwL (astPos $ head other) $ "let called with invalid args " ++ show other

defineUserFunction :: AST -> [AST] -> LContext LFunction
defineUserFunction AST { astNode = ASTSymbol param } exprs = return fn where
    fn :: LFunction
    fn env arg = do
        let replacedExprs = map (traverseAndReplace param arg) exprs
        let letExprs = take (length exprs - 1) replacedExprs
        letSymValPairs <- letExprs
                $> mapM (\case AST { astNode = ASTFunctionCall v } -> return $ drop 1 v
                               ast -> throwL (astPos ast) $ "unreachable: map letExprs, ast: " ++ show ast)
                .> fmap (mapM $ letArgsToSymValPairs env) .> join
        let body = head $ drop (length exprs - 1) replacedExprs
        let newBody = traverseAndReplace param arg body
                $> foldSymValPairs letSymValPairs
        (_, ret) <- evaluate env newBody
        return ret

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
    fn _ arg = do
        let newExprs = map (traverseAndReplace param arg) exprs
        ret <- defineUserFunctionWithLetExprs rest newExprs
        -- the returned AST will not have the correct position info, but that's fine
        -- because the info is overridden in evaluateFunctionDef anyway
        return $ makeNonsenseAST $ ASTFunction $ ret
defineUserFunctionWithLetExprs (param:_) _ = throwL (astPos $ param)
    $ "unreachable: defineUserFunctionWithLetExprs, param: " ++ show param

evaluateFunctionDef :: Env -> [AST] -> LContext (Env, AST)
evaluateFunctionDef env asts = do
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

    fn <- defineUserFunctionWithLetExprs params exprs
    return $ (env, defAst { astNode = ASTFunction fn })
    where
        isLetAST AST { astNode = ASTFunctionCall (AST { astNode = ASTSymbol "let" }:_) } = True
        isLetAST _ = False
        isParamNameShadowing name = M.member name env

evaluateMatch :: Env -> [AST] -> LContext (Env, AST)
evaluateMatch env asts = do
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

    (_, evaledActual) <- evaluate env actualExpr
    ret <- matchPairs (actualExpr, evaledActual) pairs
    return (env, matchAst { astNode = astNode ret })
    where
        matchPairs :: (AST, AST) -> [(AST, AST)] -> LContext AST
        matchPairs (actualExpr, evaledActual) [] = throwL (astPos actualExpr)
            $ "matching case not found when matching on expression: " ++ show actualExpr
            ++ " (actual value: " ++ show evaledActual ++ ")"
        matchPairs (actualExpr, evaledActual) ((matcher, branch):restPairs) = do
            (_, evaledMatcher) <- evaluate env matcher
            if evaledActual == evaledMatcher
                then do
                    (_, ret) <- evaluate env branch
                    return ret
                else matchPairs (actualExpr, evaledActual) restPairs

evaluateLet :: Env -> [AST] -> LContext (Env, AST)
evaluateLet env asts = do
    let letAst = head asts
        args = tail asts
    (symbol, value) <- letArgsToSymValPairs env args
    when (M.member symbol env) $ throwL (astPos letAst) $ "symbol already defined: " ++ symbol
    let newEnv = M.insert symbol value env
    return $ (newEnv, letAst { astNode = ASTUnit })

evaluateEnv :: Env -> [AST] -> LContext (Env, AST)
evaluateEnv env asts = do
    let envAst = head asts
    let pairs = M.assocs env
    let longestKey = L.maximumBy (compare `on` (length . fst)) pairs $> fst
    let pad s = s ++ take (length longestKey + 4 - length s) (L.repeat ' ')
    let rows = pairs $> map (\(k, v) -> pad k ++ show v)
    liftIO $ mapM_ putStrLn rows
    return (env, envAst { astNode = ASTUnit })

evaluateUserFunction :: Env -> [AST] -> LContext (Env, AST)
evaluateUserFunction env children = do
    let fnAst = head children
        args = tail children
    (_, fnEvaled) <- evaluate env fnAst
    AST { astNode = (ASTFunction fn) } <- assertIsASTFunction fnEvaled
    evaledArgs' <- mapM (evaluate env) args
    let evaledArgs = map snd evaledArgs'
    doubleEvaledArgs' <- mapM (evaluate env) evaledArgs
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

evaluate :: Env -> AST -> LContext (Env, AST)
evaluate env AST { astNode = fnc@(ASTFunctionCall args@(x:_)) } =
     do config <- ask
        when (configPrintCallStack config) $ liftIO $ putStrLn $ "fn call: " ++ show fnc
        case astNode x of
            ASTSymbol "\\" ->
                evaluateFunctionDef env args
            ASTSymbol "match" ->
                evaluateMatch env args
            ASTSymbol "let" ->
                evaluateLet env args
            ASTSymbol "env" ->
                evaluateEnv env args
            _ ->
                evaluateUserFunction env args
evaluate env ast@AST { astNode = (ASTSymbol _) } =
    evaluateSymbol env ast
evaluate env ast@AST { astNode = (ASTVector vec) } =
     do rets <- mapM (evaluate env) vec
        let vec' = map snd rets
        return $ (env, ast { astNode = ASTVector vec' })
evaluate env other =
    return (env, other)
