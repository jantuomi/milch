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
                when (stringLength == -1) $ throwError (LException "Unbalanced string literal")
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
        $ throwError $ LException "Unbalanced function call"
    when (ASTSymbol "[" `elem` asts && "[" `notElem` allowed)
        $ throwError $ LException "Unbalanced vector"
    when (ASTSymbol "{" `elem` asts && "{" `notElem` allowed)
        $ throwError $ LException "Unbalanced hash map"
    return asts

asPairsM :: [a] -> LContext [(a, a)]
asPairsM [] = return []
asPairsM (a:b:rest) = do
    restPaired <- asPairsM rest
    return $ (a, b) : restPaired
asPairsM _ = throwError $ LException "Odd number of elements to pair up"

asPairs :: [a] -> [(a, a)]
asPairs [] = []
asPairs (a:b:rest) =
    let restPaired = asPairs rest
     in (a, b) : restPaired
asPairs _ = error "Odd number of elements to pair up"

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

assertFunctionAST :: AST -> LContext AST
assertFunctionAST ast = case ast of
    (ASTFunction _) -> return ast
    _ -> throwError $ LException $ show ast ++ " is not a function"

assertIntegerAST :: AST -> LContext AST
assertIntegerAST ast = case ast of
    (ASTInteger _) -> return ast
    _ -> throwError $ LException $ show ast ++ " is not an integer"

assertSymbolAST :: AST -> LContext AST
assertSymbolAST ast = case ast of
    (ASTSymbol _) -> return ast
    _ -> throwError $ LException $ show ast ++ " is not a symbol"

assertVectorAST :: AST -> LContext AST
assertVectorAST ast = case ast of
    (ASTVector _) -> return ast
    _ -> throwError $ LException $ show ast ++ " is not a vector"

assertFunctionCallAST :: AST -> LContext AST
assertFunctionCallAST ast = case ast of
    (ASTFunctionCall _) -> return ast
    _ -> throwError $ LException $ show ast ++ " is not a function call or body"

curryCall :: [AST] -> (AST -> LContext AST) -> LContext AST
curryCall [] f = return $ ASTFunction f
curryCall (arg:[]) f = f arg
curryCall (arg:rest) f = do
    g <- curryCall rest f
    case g of
        ASTFunction f' -> f' arg
        other -> throwError $ LException $ "Cannot call value " ++ show other ++ " as a function"

type Env = M.Map String AST
builtinEnv :: Env
builtinEnv = M.fromList [
    ("+", builtinAdd2),
    ("-", builtinSubtract2),
    ("head", builtinHead),
    ("tail", builtinTail)
    ]

builtinAdd2 :: AST
builtinAdd2 =
    let outer ast1 = do
            (ASTInteger a) <- assertIntegerAST ast1
            let inner ast2 = do
                    (ASTInteger b) <- assertIntegerAST ast2
                    return $ ASTInteger $ a + b
            return $ ASTFunction $ inner
     in ASTFunction outer

builtinSubtract2 :: AST
builtinSubtract2 =
    let outer ast1 = do
            (ASTInteger a) <- assertIntegerAST ast1
            let inner ast2 = do
                    (ASTInteger b) <- assertIntegerAST ast2
                    return $ ASTInteger $ a - b
            return $ ASTFunction $ inner
     in ASTFunction outer

builtinHead :: AST
builtinHead =
    let outer ast = do
            (ASTVector vec) <- assertVectorAST ast
            when (length vec == 0) $ throwError $ LException $ "head of empty vector"
            return $ head vec
     in ASTFunction outer

builtinTail :: AST
builtinTail =
    let outer ast = do
            (ASTVector vec) <- assertVectorAST ast
            when (length vec == 0) $ throwError $ LException $ "tail of empty vector"
            return $ ASTVector $ tail vec
     in ASTFunction outer

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
makeUserDefFn _ _ _ = error $ "unreachable"

curriedMakeUserDefFn :: Env -> [AST] -> AST -> AST -> LContext AST
curriedMakeUserDefFn _ [] _ = error "unreachable"
curriedMakeUserDefFn env (param:[]) body = makeUserDefFn env param body
curriedMakeUserDefFn env ((ASTSymbol param):rest) body =
    let fn :: AST -> LContext AST
        fn arg = do
            let newBody = traverseAndReplace param arg body
            let ret = curriedMakeUserDefFn env rest newBody
            return $ ASTFunction $ ret
     in fn
curriedMakeUserDefFn _ _ _ = error $ "unreachable"

evaluate :: Env -> AST -> LContext AST
evaluate env (ASTFunctionCall (first:args))
    | first == ASTSymbol "\\" = do
        (arg1, arg2) <- case args of
                [a, b] -> return (a, b)
                _ -> throwError $ LException $ "\\ called with " ++ show (length args) ++ " arguments"
        (ASTVector params') <- assertVectorAST arg1
        params <- mapM assertSymbolAST params'
        when (length params == 0) $ throwError $ LException $ "Function must have > 0 parameters"
        body <- assertFunctionCallAST arg2
        let fn = curriedMakeUserDefFn env params body
        return $ ASTFunction fn
    | first == ASTSymbol "match" =
        throwError $ LException "match not implemented"
    | otherwise = do
        fnEvaled <- evaluate env first
        (ASTFunction fn) <- assertFunctionAST fnEvaled
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
