module FlattenTitle
  ( flattenTitle
  ) where

spToU = map f where
    f ' ' = '_'
    f x = x

flattenTitle =
    filter (`elem` '_':['a'..'z']++['1'..'9']) .
    takeWhile (/= '(') . map toLower . spToU
