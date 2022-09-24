{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Utils where

(.>) = flip (.)
($>) = flip ($)

infixr 6 $>

oddElems :: [a] -> [a]
oddElems [] = []
oddElems (x:xs) = x:evenElems xs

evenElems :: [a] -> [a]
evenElems [] = []
evenElems (_:xs) = oddElems xs
