{-# LANGUAGE LambdaCase #-}
module Evaluator (
    evaluate,
) where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Function ( on )
import Control.Monad.Except
import Utils

_curryCall :: Env -> [AST] -> LFunction -> LContext AST
_curryCall _ [] f = return $ ASTFunction f
_curryCall env (arg:[]) f = f env arg
_curryCall env (arg:rest) f = do
    g <- _curryCall env rest f
    case g of
        ASTFunction f' -> f' env arg
        other -> throwL $ "cannot call value " ++ show other ++ " as a function"

curryCall :: Env -> [AST] -> LFunction -> LContext AST
curryCall env [] f = f env ASTUnit
curryCall env args f = _curryCall env args f

traverseAndReplace :: String -> AST -> AST -> AST
traverseAndReplace param arg ast@(ASTSymbol sym)
    | sym == param = arg
    | otherwise = ast
traverseAndReplace param arg (ASTFunctionCall body) =
    ASTFunctionCall $ (map (traverseAndReplace param arg) body)
traverseAndReplace param arg (ASTVector vec) =
    ASTVector $ (map (traverseAndReplace param arg) vec)
traverseAndReplace param arg (ASTHashMap hmap) =
    ASTHashMap $ M.assocs hmap
        $> L.concatMap (\(a, b) -> [a, b])
        .> map (traverseAndReplace param arg)
        .> asPairs .> M.fromList
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
        [ASTSymbol symbol', value'] -> do
            (_, evaledValue) <- evaluate env value'
            return (symbol', evaledValue)
        [ASTSymbol "lazy", ASTSymbol symbol', value'] -> do
            return (symbol', value')
        other -> throwL $ "let called with invalid args " ++ show other

defineUserFunction :: AST -> [AST] -> LContext LFunction
defineUserFunction (ASTSymbol param) exprs = return fn where
    fn :: LFunction
    fn env arg = do
        let replacedExprs = map (traverseAndReplace param arg) exprs
        let letExprs = take (length exprs - 1) replacedExprs
        letSymValPairs <- letExprs
                $> mapM (\case (ASTFunctionCall v) -> return $ drop 1 v
                               _ -> throwL $ "unreachable: map letExprs")
                .> fmap (mapM $ letArgsToSymValPairs env) .> join
        let body = head $ drop (length exprs - 1) replacedExprs
        let newBody = traverseAndReplace param arg body
                $> foldSymValPairs letSymValPairs
        (_, ret) <- evaluate env newBody
        return ret

defineUserFunction _ _ = throwL $ "unreachable: defineUserFunction"

defineUserFunctionWithLetExprs :: [AST] -> [AST] -> LContext LFunction
defineUserFunctionWithLetExprs [] exprs =
    defineUserFunction (ASTSymbol "_") exprs
defineUserFunctionWithLetExprs (param:[]) exprs =
    defineUserFunction param exprs
defineUserFunctionWithLetExprs ((ASTSymbol param):rest) exprs = return fn where
    fn :: LFunction
    fn _ arg = do
        let newExprs = map (traverseAndReplace param arg) exprs
        ret <- defineUserFunctionWithLetExprs rest newExprs
        return $ ASTFunction $ ret
defineUserFunctionWithLetExprs _ _ = throwL $ "unreachable: defineUserFunctionWithLetExprs"

evaluateFunctionDef :: Env -> [AST] -> LContext (Env, AST)
evaluateFunctionDef env args = do
    (params'', exprs) <- case args of
        args'
            | length args' < 2 ->
                throwL $ "\\ called with " ++ show (length args) ++ " arguments"
            | otherwise -> return $ (head args', tail args')
    (ASTVector params') <- assertIsASTVector params''
    params <- mapM assertIsASTSymbol params'

    let letExprs = take (length exprs - 1) exprs
    when (any (\case ASTFunctionCall (ASTSymbol "let":_) -> False; _ -> True) letExprs)
        $ throwL "non-let expression in function definition before body"

    fn <- defineUserFunctionWithLetExprs params exprs
    return $ (env, ASTFunction fn)

evaluateMatch :: Env -> [AST] -> LContext (Env, AST)
evaluateMatch env args = do
    (cond, rest) <- case args of
            [] -> throwL $ "match called with no arguments"
            (_:[]) -> throwL $ "empty match cases"
            (a:b) -> return (a, b)
    if length rest `mod` 2 == 0
        then do
            caseMatchers' <- oddElems rest $> mapM (evaluate env)
            let caseMatchers = map snd caseMatchers'
            let caseBranches = evenElems rest
            let caseMap = M.fromList $ L.zip caseMatchers caseBranches
            (_, evaledCond) <- evaluate env cond
            case M.lookup evaledCond caseMap of
                Just branch -> evaluate env branch
                Nothing -> throwL $ "matching case not found when matching on value: " ++ show cond
        else do
            let (defaultBranch, revCases) = case reverse rest of
                    (a:b) -> (a, b)
                    _ -> error $ "unreachable: reverse rest"
            caseMatchers' <- oddElems (reverse revCases) $> mapM (evaluate env)
            let caseMatchers = map snd caseMatchers'
            let caseBranches = evenElems (reverse revCases)
            let caseMap = M.fromList $ L.zip caseMatchers caseBranches
            (_, evaledCond) <- evaluate env cond
            case M.lookup evaledCond caseMap of
                Just branch -> evaluate env branch
                Nothing -> evaluate env defaultBranch

evaluateLet :: Env -> [AST] -> LContext (Env, AST)
evaluateLet env args = do
    (symbol, value) <- letArgsToSymValPairs env args
    when (M.member symbol env) $ throwL $ "symbol already defined: " ++ symbol
    let newEnv = M.insert symbol value env
    return $ (newEnv, ASTUnit)

evaluateEnv :: Env -> LContext (Env, AST)
evaluateEnv env = do
    let pairs = M.assocs env
    let longestKey = L.maximumBy (compare `on` (length . fst)) pairs $> fst
    let pad s = s ++ take (length longestKey + 4 - length s) (L.repeat ' ')
    let rows = pairs $> map (\(k, v) -> pad k ++ show v)
    liftIO $ mapM_ putStrLn rows
    return (env, ASTUnit)

evaluateUserFunction :: Env -> [AST] -> LContext (Env, AST)
evaluateUserFunction env args = do
    let first = head args
    (_, fnEvaled) <- evaluate env first
    (ASTFunction fn) <- assertIsASTFunction fnEvaled
    evaledArgs' <- mapM (evaluate env) args
    let evaledArgs = map snd evaledArgs'
    doubleEvaledArgs' <- mapM (evaluate env) evaledArgs
    let doubleEvaledArgs = map snd doubleEvaledArgs'
    result <- curryCall env (reverse doubleEvaledArgs) fn
    return (env, result)

evaluateSymbol :: Env -> String -> LContext (Env, AST)
evaluateSymbol env sym = do
    let val = M.lookup sym env
    case val of
        Just ast -> return (env, ast)
        Nothing -> throwL $ "symbol " ++ sym ++ " not defined in environment"

evaluate :: Env -> AST -> LContext (Env, AST)
evaluate env (ASTFunctionCall (first:args)) = case first of
    ASTSymbol "\\" ->
        evaluateFunctionDef env args
    ASTSymbol "match" ->
        evaluateMatch env args
    ASTSymbol "let" ->
        evaluateLet env args
    ASTSymbol "env" ->
        evaluateEnv env
    _ ->
        evaluateUserFunction env args
evaluate env (ASTSymbol sym) =
    evaluateSymbol env sym
evaluate env other =
    return (env, other)
