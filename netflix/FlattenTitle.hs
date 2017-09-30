module FlattenTitle
  ( flattenTitle
  ) where

import Data.Char
import Data.List.Utils

spToU = map f where
    f ' ' = '_'
    f x = x

flattenTitle =
    filter (`elem` '_':['a'..'z']++['1'..'9']) .
    takeWhile (/= '(') .
    replace " " "_" .
    replace "&" "and" .
    map toLower
