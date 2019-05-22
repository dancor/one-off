{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Data.List
import qualified Data.List.Split as Spl
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import System.FilePath
import System.Process
import qualified Text.ParserCombinators.Parsec as Psec

import Color
import Coord

type BoardOf a = VM.IOVector a

type Board = BoardOf (Maybe Color)

newBoardOf :: a -> IO (BoardOf a)
newBoardOf = VM.replicate (19 * 19)

bIndex :: Coord -> Int
bIndex (Coord column row) = 19 * row + column

bRead :: BoardOf a -> Coord -> IO a
bRead b = VM.read b . bIndex

bWrite :: BoardOf a -> Coord -> a -> IO ()
bWrite b = VM.write b . bIndex

class ShowSq a where
  showSq :: a -> Char

instance ShowSq Color where
  showSq Black = '●'
  showSq White = '℗'

instance ShowSq (Either Int Color) where
  showSq (Left n) = if n >= 0 && n <= 9 then head (show n) else '?'
  showSq (Right color) = showSq color

showBoard :: ShowSq a => BoardOf (Maybe a) -> IO String
showBoard bd = do
    v <- V.freeze bd
    return $ intercalate "\n" $
        [headerFooter] ++
        zipWith doLine blankBoardLines (Spl.chunksOf 19 $ V.toList v) ++
        [headerFooter]
  where
    doSq _ (Just x) = showSq x
    doSq bg _ = bg
    doLine blankL l =
        zipWith doSq blankL ([n, n] ++ intercalate [n] (map (:[]) l) ++ [n, n])
    n = Nothing
    headerFooter = "  A B C D E F G H J K L M N O P Q R S T"
    blankBoardLines =
        [ "19┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐19"
        , "18├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤18"
        , "17├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤17"
        , "16├─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┤16"
        , "15├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤15"
        , "14├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤14"
        , "13├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤13"
        , "12├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤12"
        , "11├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤11"
        , "10├─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┤10"
        , " 9├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤9"
        , " 8├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤8"
        , " 7├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤7"
        , " 6├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤6"
        , " 5├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤5"
        , " 4├─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┤4"
        , " 3├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤3"
        , " 2├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤2"
        , " 1└─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘1"
        ]
