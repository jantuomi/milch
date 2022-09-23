{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Utils where

(.>) = flip (.)
($>) = flip ($)

infixr 6 $>
