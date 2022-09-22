module Lib
    ( runScriptFile
    ) where

import qualified Data.Map as M
import qualified Data.Bifunctor as B
import Debug.Trace

data AST
    = ASTNumber Double
    | ASTSymbol String
    | ASTBoolean Bool
    | ASTString String
    | ASTVector [AST]
    | ASTHashMap (M.Map AST AST)
    | ASTFunction (AST -> AST)

_tokenize :: [String] -> String -> String -> [String]
_tokenize acc current src = case src of
    "" -> reverse current : acc
    (x:xs)
        | x == ';' ->
            let commentDropped = dropWhile (\c -> c /= '\n') xs
             in _tokenize (reverse current : acc) "" commentDropped
        | x == '"' ->
            let consume :: String -> (String, Int)
                consume str = case str of
                    ('\\':'"':rest) -> B.bimap ('\"' :) (+ 2) (consume rest)
                    ('\\':'n':rest) -> B.bimap ('\n' :) (+ 2) (consume rest)
                    ('\\':'t':rest) -> B.bimap ('\t' :) (+ 2) (consume rest)
                    ('"':_) -> ("", 1)
                    (c:rest) -> B.bimap (c :) (+ 1) (consume rest)
                    [] -> error "Unbalanced string literal"
                (string, stringLength) = consume xs
                stringDropped = drop (stringLength) xs
             in _tokenize (string : acc) "" stringDropped
        | x `elem` [' ', '\n', '\t', '\r'] ->
            _tokenize (reverse current : acc) "" xs
        | x `elem` ['(', ')', '[', ']', '{', '}', '\\'] ->
            _tokenize ([x] : acc) "" xs
        | otherwise ->
            _tokenize acc (x : current) xs

tokenize :: String -> [String]
tokenize = filter (\t -> length t > 0) . reverse . _tokenize [] ""

parse :: [String] -> [AST]
parse src = []

runScriptFile :: String -> IO ()
runScriptFile fileName = do
    src <- readFile fileName
    let tokenized = tokenize src
    putStrLn $ "tokenized:\t\t" ++ show tokenized
    let parsed = parse tokenized
    return ()
