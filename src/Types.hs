{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Types where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.List as L
import Utils

newtype LException = LException String
data Config = Config {
    configScriptFileName :: Maybe String,
    configVerboseMode :: Bool,
    configShowHelp :: Bool
}

type LContext a = ReaderT Config (ExceptT LException IO) a

data AST
    = ASTNumber Double
    | ASTSymbol String
    | ASTBoolean Bool
    | ASTString String
    | ASTVector [AST]
    | ASTFunctionCall [AST]
    | ASTHashMap (M.Map AST AST)
    | ASTFunction (AST -> AST)

instance (Show AST) where
    show (ASTNumber n) = show n
    show (ASTSymbol s) = s
    show (ASTBoolean b) = show b
    show (ASTString s) = show s
    show (ASTVector v) = "[" ++ L.intercalate " " (map show v) ++ "]"
    show (ASTFunctionCall v) = "(" ++ L.intercalate " " (map show v) ++ ")"
    show (ASTHashMap m) =
        let flattenMap = M.assocs .> map (\(k, v) -> [k, v]) .> concat
         in "{" ++ L.intercalate " " (map show $ flattenMap m) ++ "}"
    show (ASTFunction _) = "<fn>"

instance (Eq AST) where
    ASTNumber a == ASTNumber b = a == b
    ASTSymbol a == ASTSymbol b = a == b
    ASTBoolean a == ASTBoolean b = a == b
    ASTString a == ASTString b = a == b
    ASTVector a == ASTVector b = a == b
    ASTHashMap a == ASTHashMap b = a == b
    _ == _ = False

instance (Ord AST) where
    ASTNumber a <= ASTNumber b = a <= b
    ASTSymbol a <= ASTSymbol b = a <= b
    ASTBoolean a <= ASTBoolean b = a <= b
    ASTString a <= ASTString b = a <= b
    ASTVector a <= ASTVector b = a <= b
    ASTHashMap a <= ASTHashMap b = a <= b
    _ <= _ = False
