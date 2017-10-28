module Title
  ( Title(..)
  , flattenTitle
  ) where

import Data.Char
import Data.List.Utils

data Title = Title String
  deriving (Eq, Ord, Show)

spToU = map f where
    f ' ' = '_'
    f x = x

flattenTitle = Title .
    filter (`elem` '_':['a'..'z']++['1'..'9']) .
    takeWhile (/= '(') .
    replace " " "_" .
    replace "&" "and" .
    map toLower
