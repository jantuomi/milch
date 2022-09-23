module Lib (
    runScriptFile,
    LContext
) where

import qualified Data.Map as M
import qualified Data.Bifunctor as B
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Debug.Trace
import Types
import Utils

data AST
    = ASTNumber Double
    | ASTSymbol String
    | ASTBoolean Bool
    | ASTString String
    | ASTVector [AST]
    | ASTHashMap (M.Map AST AST)
    | ASTFunction (AST -> AST)

instance (Show AST) where
    show (ASTNumber n) = show n
    show (ASTSymbol s) = show s
    show (ASTBoolean b) = show b
    show (ASTString s) = show s
    show (ASTVector v) = "[" ++ intercalate " " (map show v) ++ "]"
    show (ASTHashMap m) =
        let flattenMap = M.assocs .> map (\(k, v) -> [k, v]) .> concat
         in "[" ++ intercalate " " (map show $ flattenMap m) ++ "]"
    show (ASTFunction _) = "<fn>"

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
             in do
                when (stringLength == -1) $ throwError (LException "Unbalanced string literal")
                _tokenize (string : acc) "" stringDropped
        | x `elem` [' ', '\n', '\t', '\r'] ->
            _tokenize (reverse current : acc) "" xs
        | x `elem` ['(', ')', '[', ']', '{', '}', '\\'] ->
            _tokenize ([x] : acc) "" xs
        | otherwise ->
            _tokenize acc (x : current) xs

tokenize :: String -> LContext [String]
tokenize src = do
    tokens <- _tokenize [] "" src
    return $ tokens
        $> reverse
        .> filter (\s -> length s > 0)

parse :: [String] -> [AST]
parse src = []

runScriptFile :: String -> LContext ()
runScriptFile fileName = do
    src <- liftIO $ readFile fileName
    tokenized <- tokenize src
    config <- ask
    when (configVerboseMode config) $ liftIO $ putStrLn $ "tokenized:\t\t" ++ show tokenized
    let parsed = parse tokenized
    return ()
