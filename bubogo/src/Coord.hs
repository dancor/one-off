module Coord where

import Control.Monad
import Data.Char
import Data.Maybe
import GHC.Int (Int32)
import qualified Text.ParserCombinators.Parsec as Psec

data Coord = Coord
  { cColumn :: !Int32
  , cRow    :: !Int32
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
        | otherwise = Nothing
      rowMb = readMb rowStr
  in case (columnMb, rowMb) of
    (Just column, Just row) -> Just $ Coord (fromIntegral column) (19 - row)
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
        let row = 19 - rowPreFlip
        when (column < 0 || column >= 19 || row < 0 || row >= 19) $
            fail "Out of range"
        return $ Coord (fromIntegral column) row
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

showColumn :: Int32 -> String
showColumn x = [chr (fromIntegral x + ord 'A' + if x >= 8 then 1 else 0)]

showRow :: Int32 -> String
showRow y = show (19 - y)

showCoord :: Coord -> String
showCoord coord = showColumn (cColumn coord) ++ showRow (cRow coord)
