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
import Control.Concurrent.STM
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
import Color
import Coord
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

bgColor, boardColor, lineColor, whiteColor, blackColor :: V4 Byte
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

{-
data WinSt = WinSt
  { (Int, Int, Int)
-}

genWindowContent :: Texture -> Int -> Int -> IO (Int, Int, Int)
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
    (boardLeftX, boardTopY, cellSize) <- genWindowContent texture winW winH
    copy renderer texture Nothing Nothing
    present renderer

    engineMoveQueue <- atomically newTQueue   
    userMoveQueue <- atomically newTQueue   
    let go e = do
            userMove <- atomically $ readTQueue userMoveQueue
            ePlayMove e userMove
            engineMove <- eGenMove e White
            atomically $ writeTQueue engineMoveQueue engineMove
            go e
    forkIO $ withEngine $ \e -> setTime e >> go e
    appLoop (boardLeftX, boardTopY, cellSize, engineMoveQueue, userMoveQueue,
        renderer, texture)

posToCell (boardLeftX, boardTopY, cellSize) (P (V2 x y)) =
    if cellX >= 0 && cellX <= 18 && cellY >= 0 && cellY <= 18
    then Just (cellX, cellY) else Nothing
  where
    cellX = (toI x - boardLeftX) `div` cellSize
    cellY = (toI y - boardTopY)  `div` cellSize

-- appLoop :: WinSt -> Renderer -> Texture -> IO ()
appLoop st@(boardLeftX, boardTopY, cellSize, engineMoveQueue, userMoveQueue,
        renderer, texture) = do
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
            posToCell (boardLeftX, boardTopY, cellSize) pos
          _ -> Nothing
        quitDue = any isQPress events
        presentDue = any needsPresent events
    when presentDue $ present renderer
    case catMaybes $ map procPress events of
      (x,y):_ -> do
          atomically $ writeTQueue userMoveQueue $ Move Black $ Coord y x
          engineMove <- atomically $ readTQueue engineMoveQueue
          print engineMove
      _ -> return ()
    threadDelay 200000
    unless quitDue $ appLoop st
