{-# LANGUAGE LambdaCase #-}
module Evaluator (
    evaluate,
) where

import qualified Data.Map as M
import qualified Data.List as L
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

evalLetExpr :: Env -> [AST] -> LContext (String, AST)
evalLetExpr env args =
    case args of
        [ASTSymbol symbol', value'] -> do
            (_, evaledValue) <- evaluate env value'
            return (symbol', evaledValue)
        [ASTSymbol "lazy", ASTSymbol symbol', value'] -> do
            return (symbol', value')
        other -> throwL $ "let called with invalid args " ++ show other

foldSymValPairs :: [(String, AST)] -> AST -> AST
foldSymValPairs [] body = body
foldSymValPairs ((sym, val):rest) body =
    let replacedRestVals = map (snd .> traverseAndReplace sym val) rest
        replacedRest = zip (map fst rest) (replacedRestVals)
        replacedBody = traverseAndReplace sym val body
     in foldSymValPairs replacedRest replacedBody

makeUserDefFn :: AST -> [AST] -> LFunction
makeUserDefFn (ASTSymbol param) exprs =
    let fn :: LFunction
        fn env arg = do
            let replacedExprs = map (traverseAndReplace param arg) exprs
            let letExprs = take (length exprs - 1) replacedExprs
            letSymValPairs <- letExprs
                    $> map (\case (ASTFunctionCall v) -> drop 1 v
                                  _ -> error $ "unreachable: map letExprs")
                    .> mapM (evalLetExpr env)
            let body = head $ drop (length exprs - 1) replacedExprs
            let newBody = traverseAndReplace param arg body
                    $> foldSymValPairs letSymValPairs
            (_, ret) <- evaluate env newBody
            return ret
     in fn
makeUserDefFn _ _ = error $ "unreachable: makeUserDefFn"

curriedMakeUserDefFn :: [AST] -> [AST] -> LFunction
curriedMakeUserDefFn [] exprs = makeUserDefFn (ASTSymbol "_") exprs
curriedMakeUserDefFn (param:[]) exprs = makeUserDefFn param exprs
curriedMakeUserDefFn ((ASTSymbol param):rest) exprs =
    let fn :: LFunction
        fn _ arg = do
            let newExprs = map (traverseAndReplace param arg) exprs
            let ret = curriedMakeUserDefFn rest newExprs
            return $ ASTFunction $ ret
     in fn
curriedMakeUserDefFn _ _ = error $ "unreachable: curriedMakeUserDefFn"

evaluate :: Env -> AST -> LContext (Env, AST)
evaluate env (ASTFunctionCall (first:args))
    | first == ASTSymbol "\\" = do
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

        let fn = curriedMakeUserDefFn params exprs
        return $ (env, ASTFunction fn)
    | first == ASTSymbol "match" = do
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
    | first == ASTSymbol "let" = do
        (symbol, value) <- evalLetExpr env args
        when (M.member symbol env) $ throwL $ "symbol already defined: " ++ symbol
        let newEnv = M.insert symbol value env
        return $ (newEnv, ASTUnit)
    | first == ASTSymbol "env" = do
        liftIO $ putStrLn $ show env
        return (env, ASTUnit)
    | otherwise = do
        (_, fnEvaled) <- evaluate env first
        (ASTFunction fn) <- assertIsASTFunction fnEvaled
        evaledArgs' <- mapM (evaluate env) args
        let evaledArgs = map snd evaledArgs'
        doubleEvaledArgs' <- mapM (evaluate env) evaledArgs
        let doubleEvaledArgs = map snd doubleEvaledArgs'
        result <- curryCall env (reverse doubleEvaledArgs) fn
        return (env, result)
evaluate env (ASTSymbol sym) = do
    let val = M.lookup sym env
    case val of
        Just ast -> return (env, ast)
        Nothing -> throwL $ "symbol " ++ sym ++ " not defined in environment"
evaluate env ast = return (env, ast)
