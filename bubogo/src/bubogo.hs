{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ParallelListComp #-}

-- The higher the digit the better for the player about to play.
-- 9 means 90+% estimated chance of winning.

-- Engine bugs in AQ that we work around (I am using AQ v2.0.2),
-- but our workarounds would still work with correct engines:
-- - genmove [color]: it always ignores color and plays who it thinks is next
-- - play [color] [square]: if color is white it ignores color and plays
--   who is next
-- - use "play pass" to get around these issues

{-
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Text.ParserCombinators.Parsec as Psec
-}
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Graphics.Rendering.Cairo.Canvas hiding (toD)
import Linear (V4(..))
import Linear.V2 (V2(..))
import SDL
import SDL.Cairo
import System.IO
import System.Process

import Board
--import Color
--import Coord
import Engine
import Move

withEngine :: (Engine -> IO a) -> IO a
withEngine f = withCreateProcess ((proc "gtp-engine" [])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    ) $ \(Just inH) (Just outH) (Just errH) _engProc -> do
        hSetBuffering inH  NoBuffering
        hSetBuffering outH NoBuffering
        hSetBuffering errH NoBuffering
        f (Engine inH outH errH)

setTime :: Engine -> IO ()
setTime e = do
    ePut e "time_settings 0 1 1"
    return ()

playEngine :: IO ()
playEngine = withEngine $ \e -> setTime e >> go e [newBoardOf Nothing] [] where
    go :: Engine -> [Board] -> [Move] -> IO ()
    go _ _ _ = return ()
    {-
    goMv e b mv = do
        b2 <- bPlayMoves b [mv]
        ePlayMove e mv
        return b2
    go :: Engine -> [Board] -> [Move] -> IO ()
    go e bs@(b:_) mvs = do
        putStrLn (showBoard b) >> hFlush stdout
        gots <- getMove Black
        go2 e bs mvs gots
    go2 :: Engine -> [Board] -> [Move] -> [GetMove] -> IO ()
    go2 _ _ _ (GotQuit:_) = return ()
    go2 e (_:b:bs) (_:_:mvs) (GotUndo:gots) = do
        eSetBoard e b
        go2 e (b:bs) mvs gots
    go2 e bs mvs (GotUndo:gots) = print bs
    go2 e (b:bs) mvs [Got mv] = do
        b2 <- goMv e b mv
        putStrLn (showBoard b2) >> hFlush stdout
        eGenMove e White
        putStrLn . intercalate " " $ map showMove (mv:mvs)
        eMvMb <- eAwaitMove e White
        case eMvMb of
          Nothing -> putStrLn "Got Nothing."
          Just eMv -> do
            b3 <- bPlayMoves b2 [eMv]
            go e (b3:b:bs) (eMv:mv:mvs)
    go2 e (b:bs) mvs (Got mv:gots) = do
        b2 <- goMv e b mv
        go2 e (b2:bs) (mv:mvs) gots
    go2 e bs mvs (_:gots) = go2 e bs mvs gots
    go2 e bs mvs [] = go e bs mvs
    -}

bgColor    = V4 0xcc 0xff 0xcc 0xff
boardColor = V4 0xee 0xee 0x00 0xff
lineColor  = V4 0x00 0x00 0x00 0xff
whiteColor = V4 0xff 0xff 0xff 0xff
blackColor = V4 0x44 0x44 0x44 0xff

fromI :: Num a => Int -> a
fromI = fromIntegral

toD :: Integral a => a -> Double
toD = fromIntegral

toI :: Integral a => a -> Int
toI = fromIntegral

genWindowContent texture winW winH = do
    let sqSize = min winW winH
        cellSize = sqSize `div` 19
        halfCell = cellSize `div` 2
        boardW = cellSize * 19
        boardH = cellSize * 19
        boardLeftX = (winW - boardW) `div` 2
        boardTopY  = (winH - boardH) `div` 2
        colCenterX x = boardLeftX + halfCell + cellSize * x
        rowCenterY y = boardTopY  + halfCell + cellSize * y
        cellCenter x y = V2 (toD $ colCenterX x) (toD $ rowCenterY y)
        cellXY x y =
            V2 (toD $ colCenterX x - halfCell) (toD $ rowCenterY y - halfCell)
    withCairoTexture' texture $ runCanvas $ do
        background bgColor
        fill boardColor
        rect $ D (toD boardLeftX) (toD boardTopY) (toD boardW) (toD boardH)
        sequence_ [line (cellCenter 0 y) (cellCenter 18 y) | y <- [0..18]]
        sequence_ [line (cellCenter x 0) (cellCenter x 18) | x <- [0..18]]
        fill whiteColor
        circle (cellXY 0 0) (toD cellSize)
    return (boardLeftX, boardTopY, cellSize)

main :: IO ()
main = do
    initializeAll
    let winW = 700
        winH = 700
    window <- createWindow "Bubogo" $ defaultWindow
        { windowInitialSize = V2 (fromI winW) (fromI winH)
        , windowPosition = Absolute $ P $ V2 0 700 -- FIXME just for dev
        }
    renderer <- createRenderer window (-1) defaultRenderer
    texture <- createCairoTexture' renderer window
    winSt <- genWindowContent texture winW winH
    copy renderer texture Nothing Nothing
    present renderer
    appLoop winSt renderer texture
    --playEngine

posToCell (boardLeftX, boardTopY, cellSize) (P (V2 x y)) =
    if cellX >= 0 && cellX <= 18 && cellY >= 0 && cellY <= 18
    then Just (cellX, cellY) else Nothing
  where
    cellX = (toI x - boardLeftX) `div` cellSize
    cellY = (toI y - boardTopY)  `div` cellSize

--appLoop :: Renderer -> Texture -> IO ()
appLoop winSt renderer texture = do
    events <- pollEvents
    let isQPress event = case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
        needsPresent event = case eventPayload event of
          WindowExposedEvent _ -> True
          WindowMovedEvent _ -> True
          WindowShownEvent _ -> True
          _ -> False
        procPress event = case eventPayload event of
          MouseButtonEvent
            (MouseButtonEventData _ Pressed _ ButtonLeft _ pos) ->
            posToCell winSt pos
          _ -> Nothing
        quitDue = any isQPress events
        presentDue = any needsPresent events
    when presentDue $ present renderer
    case catMaybes $ map procPress events of
      (x,y):_ -> do
        return ()
      _ -> return ()
    threadDelay 200000
    unless quitDue $ appLoop winSt renderer texture
