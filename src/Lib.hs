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

asPairs :: [a] -> LContext [(a, a)]
asPairs [] = return []
asPairs (a:b:rest) = do
    restPaired <- asPairs rest
    return $ (a, b) : restPaired
asPairs _ = throwError $ LException "Odd number of elements to pair up"

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
    pairs <- asPairs $ reverse children
    let vec = ASTHashMap (M.fromList pairs)
    let newAcc = vec : drop (length children + 1) acc
    _parse newAcc rest
_parse acc (token:rest) =
    _parse (parseToken token : acc) rest

parse :: [String] -> LContext [AST]
parse = _parse []

isFunctionAST :: AST -> Bool
isFunctionAST ast = case ast of
    (ASTFunction _) -> True
    _ -> False

curryCall :: [AST] -> (AST -> AST) -> LContext AST
curryCall [] f = return $ ASTFunction f
curryCall (arg:[]) f = return $ f arg
curryCall (arg:rest) f = do
    g <- curryCall rest f
    case g of
        ASTFunction f' -> return $ f' arg
        other -> throwError $ LException $ "Cannot call value " ++ show other ++ " as a function"

evaluate :: AST -> LContext AST
evaluate (ASTFunctionCall (first:args)) = do
    fnEvaled <- evaluate first
    when (not $ isFunctionAST fnEvaled)
        $ throwError $ LException $ "Cannot call value " ++ show fnEvaled ++ " as a function"
    let (ASTFunction fn) = fnEvaled
    evaledArgs <- mapM evaluate args
    result <- curryCall evaledArgs fn
    return result
evaluate ast = return ast

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
    when (configVerboseMode config) $ liftIO $ mapM_ putStrLn ("parsed:" : map show parsed)
    evaluated <- mapM evaluate parsed
    liftIO $ mapM_ putStrLn (map show evaluated)
