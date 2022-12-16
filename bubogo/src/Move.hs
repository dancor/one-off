module Move where

import Color
import Coord

data Move = Pass | Move
  { mColor :: !Color
  , mCoord :: !Coord
  } deriving (Eq, Show)

showMove :: Move -> String
showMove (Move color coord) = colorLtr color ++ showCoord coord
showMove Pass = "Pass"
