module Parser (
    parse,
) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as MB
import Control.Monad.State
import Control.Monad.Except
import Text.Regex.TDFA
import Utils

validateBalance :: [String] -> [AST] -> LContext [AST]
validateBalance allowed asts = do
    when (MB.isJust parenM && "(" `notElem` allowed)
        $ throwL (astPos $ MB.fromJust parenM, "unbalanced function call")
    when (MB.isJust bracketM && "[" `notElem` allowed)
        $ throwL (astPos $ MB.fromJust bracketM, "unbalanced vector")
    when (MB.isJust curlyM && "{" `notElem` allowed)
        $ throwL (astPos $ MB.fromJust curlyM, "unbalanced hash map")
    return asts
    where
        parenM = L.find ((== ASTSymbol "(") . an) asts
        bracketM = L.find ((== ASTSymbol "[") . an) asts
        curlyM = L.find ((== ASTSymbol "{") . an) asts

parseToken :: Token -> AST
parseToken (Token token tr tc tf)
    | isTag token = let tok = tail token
                     in ast $ ASTTag (computeTagN tok) tok
    | isString token = ast $ ASTString $ removeQuotes token
    | isChar token = ast $ ASTChar $ head $ removeQuotes token
    | isInteger token = ast $ ASTInteger (read token)
    | isDouble token = ast $ ASTDouble (read token)
    | isBoolean token = ast $ ASTBoolean $ asBoolean token
    | otherwise = ast $ ASTSymbol token
    where
        integerRegex = "^-?[[:digit:]]+$"
        isInteger :: String -> Bool
        isInteger t = t =~ integerRegex
        doubleRegex = "^-?[[:digit:]]+(\\.[[:digit:]]+)?$"
        isDouble :: String -> Bool
        isDouble t = t =~ doubleRegex
        isString t = "\"" `L.isPrefixOf` t
        isChar t = "'" `L.isPrefixOf` t
        isTag t = ":" `L.isPrefixOf` t && length t > 1
        removeQuotes s = drop 1 s $> take (length s - 2)
        isBoolean t = t `elem` ["true", "false"]
        asBoolean t = t == "true"
        ast node = AST { an = node, astRow = tr, astColumn = tc, astFileName = tf }

_parse :: [AST] -> [Token] -> LContext [AST]
_parse acc' [] = do
    acc <- validateBalance [] acc'
    return $ reverse acc
_parse acc (Token { tokenContent = ")" }:rest) = do
    let children' = takeWhile (an .> (/= ASTSymbol "(")) acc
    children <- validateBalance ["("] children'
    let openParen = MB.fromJust $ L.find (an .> (== ASTSymbol "(")) acc
    let fnCall = openParen { an = ASTFunctionCall (reverse children) }
    let newAcc = fnCall : drop (length children + 1) acc
    _parse newAcc rest
_parse acc (Token { tokenContent = "]" }:rest) = do
    let children' = takeWhile (an .> (/= ASTSymbol "[")) acc
    children <- validateBalance ["["] children'
    let openBracket = MB.fromJust $ L.find (an .> (== ASTSymbol "[")) acc
    let vec = openBracket { an = ASTVector (reverse children) }
    let newAcc = vec : drop (length children + 1) acc
    _parse newAcc rest
_parse acc (Token { tokenContent = "}" }:rest) = do
    let children' = takeWhile (an .> (/= ASTSymbol "{")) acc
    children <- validateBalance ["{"] children'
    let openCurly = MB.fromJust $ L.find (an .> (== ASTSymbol "{")) acc
    pairs <- asPairsM (reverse children) `catchError` appendError (astPos openCurly, "when parsing a hash map")
    let hmap = openCurly { an = ASTHashMap (M.fromList pairs) }
    let newAcc = hmap : drop (length children + 1) acc
    _parse newAcc rest
_parse acc (token:rest) =
    _parse (parseToken token : acc) rest

parse :: [Token] -> LContext [AST]
parse = _parse []
