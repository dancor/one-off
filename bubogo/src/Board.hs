{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Control.Monad
import Data.List
import qualified Data.List.Split as Spl
import Data.Maybe
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Color
import Coord
import Move

type BoardOf a = V.Vector a
type MutBoardOf a = MV.IOVector a
type Board = BoardOf (Maybe Color)
type MutBoard = MutBoardOf (Maybe Color)

newBoardOf :: a -> BoardOf a
newBoardOf = V.replicate (19 * 19)

newMutBoardOf :: a -> IO (MutBoardOf a)
newMutBoardOf = MV.replicate (19 * 19)

bIndex :: Coord -> Int
bIndex (Coord column row) = 19 * row + column

bRead :: BoardOf a -> Coord -> a
bRead b c = b ! bIndex c

mutBRead :: MutBoardOf a -> Coord -> IO a
mutBRead b = MV.read b . bIndex

--bWrite :: BoardOf a -> Coord -> a -> BoardOf a
--bWrite b c x = b // [(bIndex c, x)]

mutBWrite :: MutBoardOf a -> Coord -> a -> IO ()
mutBWrite b = MV.write b . bIndex

tryCapture :: MutBoard -> Color -> Coord -> IO ()
tryCapture b capturedColor c = do
    v <- mutBRead b c
    when (v == Just capturedColor) $ do
        chain <- getChain b capturedColor c
        hasLib <- chainHasLiberties b chain
        unless hasLib $ captureChain b chain

mutBPlayMove :: MutBoard -> Move -> IO ()
mutBPlayMove _ Pass = return ()
mutBPlayMove b (Move color c) = do
    mutBWrite b c (Just color)
    mapM_ (tryCapture b (otherColor color)) $ coordNeighbors c

mutBPlayMoves :: MutBoard -> [Move] -> IO ()
mutBPlayMoves b = mapM_ (mutBPlayMove b)

bPlayMoves :: Board -> [Move] -> IO Board
bPlayMoves b mvs = do
  mutB <- V.thaw b
  mutBPlayMoves mutB mvs
  V.freeze mutB

-- It is assumed that the Board has Color at Coord.
getChain :: MutBoard -> Color -> Coord -> IO (MutBoardOf Bool)
getChain b color c = do
    chain <- newMutBoardOf False
    growChain b chain color c
    return chain

growChain :: MutBoard -> MutBoardOf Bool -> Color -> Coord -> IO ()
growChain b chain color c = do
    alreadySeen <- mutBRead chain c
    unless alreadySeen $ do
        v <- mutBRead b c
        when (v == Just color) $ do
            mutBWrite chain c True
            mapM_ (growChain b chain color) $ coordNeighbors c

orM :: [IO Bool] -> IO Bool
orM [] = return False
orM (x:xs) = do
    r <- x
    if r then return True else orM xs

anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM f xs = orM (map f xs)

{-
adjZ 0 = [1]
adjZ 18 = [17]
adjZ z = [z - 1, z + 1]

adjacentCoords (Coord y x) =
    map (\[y2,x2] -> Coord y2 x2) $ sequence [adjZ y, adjZ x]
-}

chainCoords :: MutBoardOf Bool -> IO [Coord]
chainCoords chain = catMaybes <$> mapM isIn allCoords
  where
    isIn c = do
        inChain <- mutBRead chain c
        return $ if inChain then Just c else Nothing

chainHasLiberties :: MutBoard -> MutBoardOf Bool -> IO Bool
chainHasLiberties b chain = anyM cHasLib =<< chainCoords chain
  where cHasLib = anyM (fmap isNothing . mutBRead b) . coordNeighbors

captureChain :: MutBoard -> MutBoardOf Bool -> IO ()
captureChain b chain = mapM_ (\c -> mutBWrite b c Nothing) =<< chainCoords chain

class ShowSq a where
  showSq :: a -> Char

instance ShowSq Color where
  showSq Black = '●'
  showSq White = '℗'

instance ShowSq (Either Int Color) where
  showSq (Left n) = if n >= 0 && n <= 9 then head (show n) else '?'
  showSq (Right color) = showSq color

showBoard :: ShowSq a => BoardOf (Maybe a) -> String
showBoard b = intercalate "\n" $ [headerFooter] ++
    zipWith doLine blankBoardLines (Spl.chunksOf 19 $ V.toList b) ++
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
