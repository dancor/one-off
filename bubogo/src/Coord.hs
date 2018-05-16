module Coord where

#include <h>

data Coord = Coord
  { cColumn :: !Int
  , cRow    :: !Int
  } deriving (Eq, Show)

readMb :: Read a => String -> Maybe a
readMb s = fmap fst . listToMaybe $ reads s

-- switch to coordParser?; better errors that way?
readCoord :: String -> Maybe Coord
readCoord (columnCh:rowStr) =
    let columnMb
          | 'A' <= columnCh && columnCh <= 'T' && columnCh /= 'I'
          = Just (ord columnCh - ord 'A' - if columnCh > 'I' then 1 else 0)
          | 'a' <= columnCh && columnCh <= 't' && columnCh /= 'i'
          = Just (ord columnCh - ord 'a' - if columnCh > 'i' then 1 else 0)
          | otherwise
          = Nothing
        rowMb = readMb rowStr
    in case (columnMb, rowMb) of
      (Just column, Just row) -> Just $ Coord column (19 - row)
      _ -> Nothing
readCoord _ = Nothing

coordParser :: Psec.Parser Coord
coordParser = do
    columnCh <- Psec.anyChar
    let columnMb
            | 'A' <= columnCh && columnCh <= 'T' && columnCh /= 'I'
            = Just $ ord columnCh - ord 'A' - if columnCh > 'I' then 1 else 0
            | 'a' <= columnCh && columnCh <= 't' && columnCh /= 'i'
            = Just $ ord columnCh - ord 'a' - if columnCh > 'i' then 1 else 0
            | otherwise
            = Nothing
    case columnMb of
      Just column -> do 
        rowPreFlip <- read <$> Psec.many1 Psec.digit
        return $ Coord column (19 - rowPreFlip)
      Nothing -> fail "Bad column"

allCoords :: [Coord]
allCoords = [Coord column row | column <- [0..18], row <- [0..18]]

coordNeighbors :: Coord -> [Coord]
coordNeighbors (Coord column row) = concat
    [ if column ==  0 then [] else [Coord (column - 1) row]
    , if column == 18 then [] else [Coord (column + 1) row]
    , if row    ==  0 then [] else [Coord column (row - 1)]
    , if row    == 18 then [] else [Coord column (row + 1)]
    ]

columnStr :: Int -> String
columnStr x = [chr (x + ord 'A' + if x >= 8 then 1 else 0)]

rowStr :: Int -> String
rowStr y = show (19 - y)

showCoord :: Coord -> String
showCoord coord = columnStr (cColumn coord) ++ rowStr (cRow coord)
