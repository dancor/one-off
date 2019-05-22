module Color where

import qualified Text.ParserCombinators.Parsec as Psec

data Color = Black | White deriving (Eq, Show)

colorLtr :: Color -> String
colorLtr Black = "b"
colorLtr White = "w"

otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black

colorParser :: Psec.Parser Color
colorParser = Psec.choice
  [ Psec.char 'b' >> return Black
  , Psec.char 'B' >> return Black
  , Psec.char 'w' >> return White
  , Psec.char 'W' >> return White
  ]
