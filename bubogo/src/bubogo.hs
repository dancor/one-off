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

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Maybe
import GHC.IO.Exception
import Graphics.Rendering.Cairo.Canvas hiding (toD)
import Linear (V4(..))
import Linear.V2 (V2(..))
import SDL
import SDL.Cairo

import Board
import Color
import Coord
import Engine
import Move

bgColor, boardColor, lineColor, whiteColor, blackColor, recentColor :: V4 Byte
bgColor     = V4 0xcc 0xff 0xcc 0xff
boardColor  = V4 0xee 0xee 0x00 0xff
lineColor   = V4 0x00 0x00 0x00 0xff
whiteColor  = V4 0xff 0xff 0xff 0xff
blackColor  = V4 0x44 0x44 0x44 0xff
recentColor = V4 0xff 0x88 0x88 0xff

fromI :: Num a => Int -> a
fromI = fromIntegral

toD :: Integral a => a -> Double
toD = fromIntegral

toI :: Integral a => a -> Int
toI = fromIntegral

data AppState = AppState
    { sWinW            :: Int
    , sWinH            :: Int
    , sBoardLeftX      :: Int
    , sBoardTopY       :: Int
    , sCellSize        :: Int
    , sEngineMoveQueue :: TQueue (Maybe Move)
    , sUserMoveQueue   :: TQueue [Move]
    , sRenderer        :: Renderer
    , sTexture         :: TVar Texture
    , sBoard           :: TVar Board
    , sRecent          :: Maybe Coord
    }

genWindowContent :: AppState -> IO AppState
genWindowContent st@AppState{sWinW=winW,sWinH=winH,sTexture=textureVar} = do
    board <- atomically $ readTVar $ sBoard st
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
        markerR = toD cellSize / 4
        adj (V2 x y) = V2 (x - markerR / 2) (y - markerR / 2)
    texture <- 
        createCairoTexture (sRenderer st) (V2 (fromI winW) (fromI winH))
    withCairoTexture' texture $ runCanvas $ do
        background bgColor
        stroke lineColor
        fill boardColor
        rect $ D (toD boardLeftX) (toD boardTopY) (toD boardW) (toD boardH)
        sequence_ [line (cellCenter 0 y) (cellCenter 18 y) | y <- [0..18]]
        sequence_ [line (cellCenter x 0) (cellCenter x 18) | x <- [0..18]]
        fill blackColor
        mapM_ (\[x,y] -> circle (adj $ cellCenter x y) markerR) $
            sequence $ replicate 2 [3,9,15]
        sequence_ [sequence_ [when (bRead board (Coord y x) == Just Black) $
            circle (cellXY x y) (toD cellSize)| x <- [0..18]] | y <- [0..18]]
        fill whiteColor
        sequence_ [sequence_ [when (bRead board (Coord y x) == Just White) $
            circle (cellXY x y) (toD cellSize)| x <- [0..18]] | y <- [0..18]]
        case sRecent st of
          Just (Coord y x) -> do
            fill recentColor
            circle (adj $ cellCenter x y) markerR
          _ -> return ()
    atomically $ writeTVar textureVar texture
    return $ st {sBoardLeftX = boardLeftX, sBoardTopY = boardTopY, 
        sCellSize = cellSize}

main :: IO ()
main = do
    initializeAll
    let winW = 700
        winH = 700
    engineMoveQueue <- atomically newTQueue   
    userMoveQueue <- atomically newTQueue   
    boardVar <- atomically $ newTVar $ newBoardOf Nothing
    textureVar <- atomically $ newTVar $ error "Texture uninitialized"
    window <- createWindow "Bubogo" $ defaultWindow
        { windowInitialSize = V2 (fromI winW) (fromI winH)
        , windowPosition = Absolute $ P $ V2 0 700 -- FIXME just for dev
        , windowResizable = True
        }
    renderer <- createRenderer window (-1) defaultRenderer
    st <- genWindowContent $ AppState winW winH 0 0 0
        engineMoveQueue userMoveQueue renderer textureVar
        boardVar Nothing
    texture <- atomically $ readTVar textureVar
    copy renderer texture Nothing Nothing
    present renderer

    let go e = do
            userMoves <- atomically $ readTQueue userMoveQueue
            let vanishRedo :: IOError -> IO (Engine, Maybe Move)
                vanishRedo exc = if ioe_type exc `elem` [EOF, ResourceVanished]
                  then do
                    putStrLn "Engine vanished; restarted."
                    eng <- startEngine
                    board <- atomically $ readTVar boardVar
                    eSetBoard eng board
                    putStrLn "Board set."
                    tryWithE eng
                  else throwIO exc
                tryWithE eng = do
                    mapM_ (ePlayMove eng) userMoves
                    (,) eng <$> eGenMove eng White
            (e2, engineMove) <- handle vanishRedo (tryWithE e)
            atomically $ writeTQueue engineMoveQueue engineMove
            go e2
    _ <- forkIO $ startEngine >>= go
    appLoop st

posToCell :: AppState -> Int -> Int -> Maybe (Int, Int)
posToCell st x y =
    if cellX >= 0 && cellX <= 18 && cellY >= 0 && cellY <= 18
    then Just (cellX, cellY) else Nothing
  where
    cellX = (x - sBoardLeftX st) `div` sCellSize st
    cellY = (y - sBoardTopY st)  `div` sCellSize st

appLoop :: AppState -> IO ()
appLoop st@AppState{sBoard=boardVar,sRenderer=renderer,sTexture=textureVar} = do
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
            (MouseButtonEventData _ Pressed _ ButtonLeft _ (P (V2 x y))) ->
            posToCell st (toI x) (toI y)
          _ -> Nothing
        procResize event = case eventPayload event of
          WindowResizedEvent (WindowResizedEventData _ (V2 x y)) ->
            Just (toI x, toI y)
          _ -> Nothing
        quitDue = any isQPress events
    (st2, renderDue, presentDue) <- case catMaybes $ map procPress events of
      (x,y):_ -> do
          board <- atomically $ readTVar boardVar
          case bRead board (Coord y x) of
            Nothing -> do
              let userMove = Move Black $ Coord y x
                  userMoves = if y == 3 && x == 3
                    then [userMove
                      , Move Black $ Coord 3 15
                      , Move Black $ Coord 15 3
                      -- , Move Black $ Coord 15 15
                      ]
                    else [userMove]
              atomically $ writeTQueue (sUserMoveQueue st) userMoves
              board2 <- bPlayMoves board userMoves
              atomically $ writeTVar boardVar board2
              _ <- genWindowContent $ st {sRecent = Just $ Coord y x}
              texture <- atomically $ readTVar textureVar
              copy renderer texture Nothing Nothing
              present renderer
              Just engineMove@(Move _ engineCoord) <-
                  atomically $ readTQueue (sEngineMoveQueue st)
              print engineMove
              board3 <- bPlayMoves board2 [engineMove]
              atomically $ writeTVar boardVar board3
              return (st {sRecent = Just engineCoord}, True, True)
            _ -> return (st, False, any needsPresent events)
      _ -> return (st, False, any needsPresent events)
    let (st3, renderDue2, presentDue2) =
          case catMaybes $ map procResize events of
            (w,h):_ -> (st2 {sWinW = w, sWinH = h}, True, True)
            _ -> (st2, renderDue, presentDue)
    st4 <- if renderDue2
      then do
        st4 <- genWindowContent st3
        texture2 <- atomically $ readTVar textureVar
        copy renderer texture2 Nothing Nothing
        return st4
      else return st3
    when presentDue2 $ present renderer
    threadDelay 200000
    unless quitDue $ appLoop st4
