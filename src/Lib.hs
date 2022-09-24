{-# LANGUAGE LambdaCase #-}
module Lib (
    runScriptFile,
    runInlineScript,
) where

import qualified Data.Map as M
import qualified Data.Bifunctor as B
import qualified Data.List as L
import Control.Monad.Except
import Control.Monad.Reader
import Text.Regex.TDFA
import Types
import Utils
import Builtins
import Debug.Trace

_tokenize :: [String] -> String -> String -> LContext [String]
_tokenize acc current src = case src of
    "" -> return $ reverse current : acc
    (x:xs)
        | x == ';' ->
            let commentDropped = dropWhile (\c -> c /= '\n') xs
             in _tokenize (reverse current : acc) "" commentDropped
        | x == '"' ->
            -- String length -1 signals an unbalanced error
            let inc k n = if n == -1 then -1 else n + k
                consume :: String -> (String, Int)
                consume str = case str of
                    ('\\':'"':rest) -> B.bimap ('\"' :) (inc 2) (consume rest)
                    ('\\':'n':rest) -> B.bimap ('\n' :) (inc 2) (consume rest)
                    ('\\':'t':rest) -> B.bimap ('\t' :) (inc 2) (consume rest)
                    ('"':_) -> ("", 1)
                    (c:rest) -> B.bimap (c :) (inc 1) (consume rest)
                    [] -> ("", -1)
                (string, stringLength) = consume xs
                stringDropped = drop (stringLength) xs
                withQuotes = "\"" ++ string ++ "\""
             in do
                when (stringLength == -1) $ throwError (LException "unbalanced string literal")
                _tokenize (withQuotes : acc) "" stringDropped
        | x `elem` [' ', '\n', '\t', '\r'] ->
            _tokenize (reverse current : acc) "" xs
        | x `elem` ['(', ')', '[', ']', '{', '}', '\\'] ->
            _tokenize ([x] : reverse current : acc) "" xs
        | otherwise ->
            _tokenize acc (x : current) xs

tokenize :: String -> LContext [String]
tokenize src = do
    tokens <- _tokenize [] "" src
    return $ tokens
        $> reverse
        .> filter (\s -> length s > 0)

validateBalance :: [String] -> [AST] -> LContext [AST]
validateBalance allowed asts = do
    when (ASTSymbol "(" `elem` asts && "(" `notElem` allowed)
        $ throwError $ LException "unbalanced function call"
    when (ASTSymbol "[" `elem` asts && "[" `notElem` allowed)
        $ throwError $ LException "unbalanced vector"
    when (ASTSymbol "{" `elem` asts && "{" `notElem` allowed)
        $ throwError $ LException "unbalanced hash map"
    return asts

asPairsM :: [a] -> LContext [(a, a)]
asPairsM [] = return []
asPairsM (a:b:rest) = do
    restPaired <- asPairsM rest
    return $ (a, b) : restPaired
asPairsM _ = throwError $ LException "odd number of elements to pair up"

asPairs :: [a] -> [(a, a)]
asPairs [] = []
asPairs (a:b:rest) =
    let restPaired = asPairs rest
     in (a, b) : restPaired
asPairs _ = error "odd number of elements to pair up"


parseToken :: String -> AST
parseToken token
    | isInteger token = ASTInteger (read token)
    | isDouble token = ASTDouble (read token)
    | isString token = ASTString $ removeQuotes token
    | isBoolean token = ASTBoolean $ asBoolean token
    | otherwise = ASTSymbol token
    where
        integerRegex = "^-?[[:digit:]]+$"
        isInteger :: String -> Bool
        isInteger t = t =~ integerRegex
        doubleRegex = "^-?[[:digit:]]+(\\.[[:digit:]]+)?$"
        isDouble :: String -> Bool
        isDouble t = t =~ doubleRegex
        isString t = "\"" `L.isPrefixOf` t
        removeQuotes s = drop 1 s $> take (length s - 2)
        isBoolean t = t `elem` ["true", "false"]
        asBoolean t = if t == "true" then True else False

_parse :: [AST] -> [String] -> LContext [AST]
_parse acc' [] = do
    acc <- validateBalance [] acc'
    return $ reverse acc
_parse acc (")":rest) = do
    let children' = takeWhile (/= ASTSymbol "(") acc
    children <- validateBalance ["("] children'
    let fnCall = ASTFunctionCall (reverse children)
    let newAcc = fnCall : drop (length children + 1) acc
    _parse newAcc rest
_parse acc ("]":rest) = do
    let children' = takeWhile (/= ASTSymbol "[") acc
    children <- validateBalance ["["] children'
    let vec = ASTVector (reverse children)
    let newAcc = vec : drop (length children + 1) acc
    _parse newAcc rest
_parse acc ("}":rest) = do
    let children' = takeWhile (/= ASTSymbol "{") acc
    children <- validateBalance ["{"] children'
    pairs <- asPairsM $ reverse children
    let vec = ASTHashMap (M.fromList pairs)
    let newAcc = vec : drop (length children + 1) acc
    _parse newAcc rest
_parse acc (token:rest) =
    _parse (parseToken token : acc) rest

parse :: [String] -> LContext [AST]
parse = _parse []

_curryCall :: Env -> [AST] -> LFunction -> LContext AST
_curryCall env [] f = return $ ASTFunction f
_curryCall env (arg:[]) f = f env arg
_curryCall env (arg:rest) f = do
    g <- _curryCall env rest f
    case g of
        ASTFunction f' -> f' env arg
        other -> throwError $ LException $ "cannot call value " ++ show other ++ " as a function"

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
        other -> throwError $ LException $ "let called with invalid args " ++ show other

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
                    $> map (\(ASTFunctionCall v) -> drop 1 v)
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
                    throwError $ LException $ "\\ called with " ++ show (length args) ++ " arguments"
                | otherwise -> return $ (head args', tail args')
        (ASTVector params') <- assertIsASTVector params''
        params <- mapM assertIsASTSymbol params'

        let letExprs = take (length exprs - 1) exprs
        when (any (\case ASTFunctionCall (ASTSymbol "let":_) -> False; _ -> True) letExprs)
            $ throwError $ LException "non-let expression in function definition before body"

        let fn = curriedMakeUserDefFn params exprs
        return $ (env, ASTFunction fn)
    | first == ASTSymbol "match" = do
        (cond, rest) <- case args of
                [] -> throwError $ LException $ "match called with no arguments"
                (_:[]) -> throwError $ LException $ "empty match cases"
                (cond':rest') -> return (cond', rest')
        if length rest `mod` 2 == 0
            then do
                caseMatchers' <- oddElems rest $> mapM (evaluate env)
                let caseMatchers = map snd caseMatchers'
                let caseBranches = evenElems rest
                let caseMap = M.fromList $ L.zip caseMatchers caseBranches
                (_, evaledCond) <- evaluate env cond
                case M.lookup evaledCond caseMap of
                    Just branch -> evaluate env branch
                    Nothing -> throwError $ LException $ "matching case not found, condition " ++ show cond
            else do
                let (defaultBranch:revCases) = reverse rest
                caseMatchers' <- oddElems (reverse revCases) $> mapM (evaluate env)
                let caseMatchers = map (\(_, a) -> a) caseMatchers'
                let caseBranches = evenElems (reverse revCases)
                let caseMap = M.fromList $ L.zip caseMatchers caseBranches
                (_, evaledCond) <- evaluate env cond
                case M.lookup evaledCond caseMap of
                    Just branch -> evaluate env branch
                    Nothing -> evaluate env defaultBranch
    | first == ASTSymbol "let" = do
        (symbol, value) <- evalLetExpr env args
        when (M.member symbol env) $ throwError $ LException $ "symbol already defined: " ++ symbol
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
        Nothing -> throwError $ LException $ "symbol " ++ sym ++ " not defined in environment"
evaluate env ast = return (env, ast)

runScriptFile :: Env -> String -> LContext Env
runScriptFile env fileName = do
    src <- liftIO $ readFile fileName
    runInlineScript env src

evalParsed :: Env -> [AST] -> LContext (Env, [AST])
evalParsed env [] = return (env, [])
evalParsed env (ast:rest) = do
    (newEnv, newAst) <- evaluate env ast
    (retEnv, restEvaled) <- evalParsed newEnv rest
    return $ (retEnv, newAst : restEvaled)

runInlineScript :: Env -> String -> LContext Env
runInlineScript env src = do
    tokenized <- tokenize src
    config <- ask
    when (configVerboseMode config) $ liftIO $ putStrLn $ "tokenized:\t\t" ++ show tokenized
    parsed <- parse tokenized
    when (configVerboseMode config) $ do
        let output = "parsed:\t\t\t" ++ (map show parsed $> L.intercalate "\n\t\t\t")
        liftIO $ putStrLn output
    (newEnv, evaluated) <- evalParsed env parsed
    liftIO $ mapM_ putStrLn (map show evaluated)
    return newEnv
