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

_curryCall :: [AST] -> (AST -> LContext AST) -> LContext AST
_curryCall [] f = return $ ASTFunction f
_curryCall (arg:[]) f = f arg
_curryCall (arg:rest) f = do
    g <- _curryCall rest f
    case g of
        ASTFunction f' -> f' arg
        other -> throwError $ LException $ "cannot call value " ++ show other ++ " as a function"

curryCall :: [AST] -> (AST -> LContext AST) -> LContext AST
curryCall [] f = f ASTUnit
curryCall args f = _curryCall args f

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

makeUserDefFn :: Env -> AST -> AST -> AST -> LContext AST
makeUserDefFn env (ASTSymbol param) body =
    let fn :: AST -> LContext AST
        fn arg = do
            let newBody = traverseAndReplace param arg body
            evaluate env newBody
     in fn
makeUserDefFn _ _ _ = error $ "unreachable: makeUserDefFn"

curriedMakeUserDefFn :: Env -> [AST] -> AST -> AST -> LContext AST
curriedMakeUserDefFn env [] body = makeUserDefFn env (ASTSymbol "_") body
curriedMakeUserDefFn env (param:[]) body = makeUserDefFn env param body
curriedMakeUserDefFn env ((ASTSymbol param):rest) body =
    let fn :: AST -> LContext AST
        fn arg = do
            let newBody = traverseAndReplace param arg body
            let ret = curriedMakeUserDefFn env rest newBody
            return $ ASTFunction $ ret
     in fn
curriedMakeUserDefFn _ _ _ = error $ "unreachable: curriedMakeUserDefFn"

evaluate :: Env -> AST -> LContext AST
evaluate env (ASTFunctionCall (first:args))
    | first == ASTSymbol "\\" = do
        (arg1, arg2) <- case args of
                [arg1', arg2'] -> return (arg1', arg2')
                _ -> throwError $ LException $ "\\ called with " ++ show (length args) ++ " arguments"
        (ASTVector params') <- assertIsASTVector arg1
        params <- mapM assertIsASTSymbol params'
        -- when (length params == 0) $ throwError $ LException $ "Function must have > 0 parameters"
        body <- assertIsASTFunctionCall arg2
        let fn = curriedMakeUserDefFn env params body
        return $ ASTFunction fn
    | first == ASTSymbol "match" = do
        (cond, rest) <- case args of
                [] -> throwError $ LException $ "match called with no arguments"
                (_:[]) -> throwError $ LException $ "empty match cases"
                (cond':rest') -> return (cond', rest')
        if length rest `mod` 2 == 0
            then do
                caseMatchers <- oddElems rest $> mapM (evaluate env)
                let caseBranches = evenElems rest
                let caseMap = M.fromList $ L.zip caseMatchers caseBranches
                evaledCond <- evaluate env cond
                case M.lookup evaledCond caseMap of
                    Just branch -> evaluate env branch
                    Nothing -> throwError $ LException $ "matching case not found, condition " ++ show cond
            else do
                let (defaultBranch:revCases) = reverse rest
                caseMatchers <- oddElems (reverse revCases) $> mapM (evaluate env)
                let caseBranches = evenElems (reverse revCases)
                let caseMap = M.fromList $ L.zip caseMatchers caseBranches
                evaledCond <- evaluate env cond
                case M.lookup evaledCond caseMap of
                    Just branch -> evaluate env branch
                    Nothing -> evaluate env defaultBranch
    | otherwise = do
        fnEvaled <- evaluate env first
        (ASTFunction fn) <- assertIsASTFunction fnEvaled
        evaledArgs <- mapM (evaluate env) args
        result <- curryCall (reverse evaledArgs) fn
        return result
evaluate env (ASTSymbol sym) = do
    let val = M.lookup sym env
    case val of
        Just ast -> return ast
        Nothing -> throwError $ LException $ "Symbol " ++ sym ++ " not defined in environment"
evaluate _ ast = return ast

runScriptFile :: String -> LContext ()
runScriptFile fileName = do
    src <- liftIO $ readFile fileName
    runInlineScript src

runInlineScript :: String -> LContext ()
runInlineScript src = do
    tokenized <- tokenize src
    config <- ask
    when (configVerboseMode config) $ liftIO $ putStrLn $ "tokenized:\t\t" ++ show tokenized
    parsed <- parse tokenized
    when (configVerboseMode config) $ do
        let output = "parsed:\t\t\t" ++ (map show parsed $> L.intercalate "\n\t\t\t")
        liftIO $ putStrLn output
    evaluated <- mapM (evaluate builtinEnv) parsed
    liftIO $ mapM_ putStrLn (map show evaluated)
