module Coord where

data Coord = Coord
  { cColumn :: !Int
  , cRow    :: !Int
  } deriving (Eq, Show)
