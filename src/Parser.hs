module Parser (
    parse,
) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as MB
import Control.Monad.Except
import Text.Regex.TDFA
import Utils

validateBalance :: [String] -> [AST] -> LContext [AST]
validateBalance allowed asts = do
    when (MB.isJust parenM && "(" `notElem` allowed)
        $ throwL (astPos $ MB.fromJust parenM) "unbalanced function call"
    when (MB.isJust bracketM && "[" `notElem` allowed)
        $ throwL (astPos $ MB.fromJust bracketM) "unbalanced vector"
    when (MB.isJust curlyM && "{" `notElem` allowed)
        $ throwL (astPos $ MB.fromJust curlyM) "unbalanced hash map"
    return asts
    where
        parenM = L.find ((== ASTSymbol "(") . astNode) asts
        bracketM = L.find ((== ASTSymbol "[") . astNode) asts
        curlyM = L.find ((== ASTSymbol "{") . astNode) asts

parseToken :: Token -> AST
parseToken (Token token tr tc tf)
    | isInteger token = ast $ ASTInteger (read token)
    | isDouble token = ast $ ASTDouble (read token)
    | isString token = ast $ ASTString $ removeQuotes token
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
        removeQuotes s = drop 1 s $> take (length s - 2)
        isBoolean t = t `elem` ["true", "false"]
        asBoolean t = t == "true"
        ast astNode = AST { astNode = astNode, astRow = tr, astColumn = tc, astFileName = tf }

_parse :: [AST] -> [Token] -> LContext [AST]
_parse acc' [] = do
    acc <- validateBalance [] acc'
    return $ reverse acc
_parse acc (Token { tokenContent = ")" }:rest) = do
    let children' = takeWhile (astNode .> (/= ASTSymbol "(")) acc
    children <- validateBalance ["("] children'
    let openParen = MB.fromJust $ L.find (astNode .> (== ASTSymbol "(")) acc
    let fnCall = openParen { astNode = ASTFunctionCall (reverse children) }
    let newAcc = fnCall : drop (length children + 1) acc
    _parse newAcc rest
_parse acc (Token { tokenContent = "]" }:rest) = do
    let children' = takeWhile (astNode .> (/= ASTSymbol "[")) acc
    children <- validateBalance ["["] children'
    let openBracket = MB.fromJust $ L.find (astNode .> (== ASTSymbol "[")) acc
    let vec = openBracket { astNode = ASTVector (reverse children) }
    let newAcc = vec : drop (length children + 1) acc
    _parse newAcc rest
_parse acc (Token { tokenContent = "}" }:rest) = do
    let children' = takeWhile (astNode .> (/= ASTSymbol "{")) acc
    children <- validateBalance ["{"] children'
    let openCurly = MB.fromJust $ L.find (astNode .> (== ASTSymbol "{")) acc
    pairs <- asPairsM (reverse children) `catchError`
        \(LException _ e) -> throwL (astPos openCurly) e
    let hmap = openCurly { astNode = ASTHashMap (M.fromList pairs) }
    let newAcc = hmap : drop (length children + 1) acc
    _parse newAcc rest
_parse acc (token:rest) =
    _parse (parseToken token : acc) rest

parse :: [Token] -> LContext [AST]
parse = _parse []