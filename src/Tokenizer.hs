module Tokenizer (
    tokenize,
    tokenize'
) where

import qualified Data.Bifunctor as B
import Control.Monad.Except
import Utils

data TChar = TChar {
    tChar :: Char,
    tRow :: Int,
    tColumn :: Int
}

posTChar :: String -> TChar -> String
posTChar fileName TChar { tRow = r, tColumn = c } = fileName ++ ":" ++ show r ++ ":" ++ show c

_tokenize :: String -> [Token] -> [TChar] -> [TChar] -> LContext [Token]
_tokenize fileName acc current [] =
    let cur = reverse current
        token = Token {
            tokenContent = cur $> map tChar,
            tokenRow = tRow $ head cur,
            tokenColumn = tColumn $ head cur,
            tokenFileName = fileName }
     in return $ token : acc
_tokenize fileName acc current (x:xs)
    | tChar x == ';' =
        let commentDropped = dropWhile (\tc -> tChar tc /= '\n') xs
            cur = reverse current
            token = Token {
                tokenContent = cur $> map tChar,
                tokenRow = tRow $ head cur,
                tokenColumn = tColumn $ head cur,
                tokenFileName = fileName }
         in _tokenize fileName (token : acc) [] commentDropped
    | tChar x == '\'' =
        -- consumedN == -1 signals an unbalanced error
        let inc k n = if n == -1 then -1 else n + k
            consume :: String -> (String, Int)
            consume str = case str of
                ('\\':'\'':rest) -> B.bimap ('\'' :) (inc 2) (consume rest)
                ('\'':_) -> ("", 1)
                (c:rest) -> B.bimap (c :) (inc 1) (consume rest)
                [] -> ("", -1)
            (char, consumedN) = consume (map tChar xs)
            charDropped = drop (consumedN) xs
            withQuotes = "\'" ++ char ++ "\'"
            token = Token {
                tokenContent = withQuotes,
                tokenRow = tRow $ x,
                tokenColumn = tColumn $ x,
                tokenFileName = fileName }
        in do
            when (consumedN == -1) $ throwL (posTChar fileName x, "unbalanced char literal")
            when (length char > 1) $ throwL (posTChar fileName x, "char literal length > 1")
            _tokenize fileName (token : acc) [] charDropped
    | tChar x == '"' =
        -- consumedN == -1 signals an unbalanced error
        let inc k n = if n == -1 then -1 else n + k
            consume :: String -> (String, Int)
            consume str = case str of
                ('\\':'"':rest) -> B.bimap ('\"' :) (inc 2) (consume rest)
                ('\\':'n':rest) -> B.bimap ('\n' :) (inc 2) (consume rest)
                ('\\':'t':rest) -> B.bimap ('\t' :) (inc 2) (consume rest)
                ('"':_) -> ("", 1)
                (c:rest) -> B.bimap (c :) (inc 1) (consume rest)
                [] -> ("", -1)
            (string, consumedN) = consume (map tChar xs)
            stringDropped = drop (consumedN) xs
            withQuotes = "\"" ++ string ++ "\""
            token = Token {
                tokenContent = withQuotes,
                tokenRow = tRow $ x,
                tokenColumn = tColumn $ x,
                tokenFileName = fileName }
        in do
            when (consumedN == -1) $ throwL (posTChar fileName x, "unbalanced string literal")
            _tokenize fileName (token : acc) [] stringDropped
    | tChar x `elem` [' ', '\n', '\t', '\r'] =
        let cur = reverse current
            token = Token {
                tokenContent = cur $> map tChar,
                tokenRow = tRow $ head cur,
                tokenColumn = tColumn $ head cur,
                tokenFileName = fileName }
         in _tokenize fileName (token : acc) [] xs
    | tChar x `elem` ['(', ')', '[', ']', '{', '}'] =
        let cur = reverse current
            token1 = Token {
                tokenContent = [tChar x],
                tokenRow = tRow x,
                tokenColumn = tColumn x,
                tokenFileName = fileName
            }
            token2 = Token {
                tokenContent = cur $> map tChar,
                tokenRow = tRow $ head cur,
                tokenColumn = tColumn $ head cur,
                tokenFileName = fileName
            }
         in _tokenize fileName (token1 : token2 : acc) [] xs
    | otherwise = _tokenize fileName acc (x : current) xs

type RowN = Int
type ColN = Int

tokenize :: String -> String -> LContext [Token]
tokenize = tokenize' (1, 1)

tokenize' :: (RowN, ColN) -> String -> String -> LContext [Token]
tokenize' (row, col) fileName src = do
    let tChars = augment row col src
    tokens <- _tokenize fileName [] [] tChars
    return $ tokens
        $> reverse
        .> filter (\t -> length (tokenContent t) > 0)

    where
        augment row' col' ('\r':'\n':rest) =
            TChar '\n' row' col' : augment (row' + 1) 1 rest
        augment row' col' ('\n':rest) =
            TChar '\n' row' col' : augment (row' + 1) 1 rest
        augment row' col' (c:rest) =
            TChar c row' col' : augment row' (col' + 1) rest
        augment _ _ [] = []
