{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Maybe
import GHC.Int (Int32)
import GHC.IO.Exception
import GHC.IO.Handle.FD (stdout)
import GHC.IO.Handle (hFlush)
import Graphics.Rendering.Cairo.Canvas hiding (toD)
import "sdl2-cairo" SDL.Cairo
import "sdl2" SDL
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
toD :: Integral a => a -> Double
toD = fromIntegral
data AppState = AppState
  { sWinW            :: Int32
  , sWinH            :: Int32
  , sBoardLeftX      :: Int32
  , sBoardTopY       :: Int32
  , sCellSize        :: Int32
  , sEngineMoveQueue :: TQueue (Maybe Move)
  , sUserMoveQueue   :: TQueue [Move]
  , sRenderer        :: Renderer
  , sTexture         :: TVar Texture
  , sBoardVar        :: TVar [Board]
  , sRecent          :: Maybe Coord
  }
genWindowContent :: AppState -> IO AppState
genWindowContent st@AppState{sWinW=winW,sWinH=winH,sTexture=textureVar} = do
  board:_ <- atomically $ readTVar $ sBoardVar st
  let sqSize = min winW winH
      cellSize = sqSize `div` 19
      halfCell = cellSize `div` 2
      boardW = cellSize * 19
      boardH = cellSize * 19
      boardLeftX = (winW - boardW) `div` 2
      boardTopY  = (winH - boardH) `div` 2
      colCenterX x = boardLeftX + halfCell + cellSize * x
      rowCenterY y = boardTopY  + halfCell + cellSize * y
      cellCenter :: Int32 -> Int32 -> V2 Double
      cellCenter x y = V2 (toD $ colCenterX x) (toD $ rowCenterY y)
      cellXY x y =
          V2 (toD $ colCenterX x - halfCell) (toD $ rowCenterY y - halfCell)
      markerR = toD cellSize / 4
      adj (V2 x y) = V2 (x - markerR / 2) (y - markerR / 2)
  texture <- createCairoTexture (sRenderer st) $
    V2 (fromIntegral winW) (fromIntegral winH)
  withCairoTexture' texture $ runCanvas $ do
    background bgColor; stroke lineColor; fill boardColor
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
      Just (Coord y x) -> fill recentColor >>
        circle (adj $ cellCenter x y) markerR
      _ -> return ()
  atomically $ writeTVar textureVar texture
  return $ st {sBoardLeftX = boardLeftX, sBoardTopY = boardTopY, 
    sCellSize = cellSize}
main :: IO ()
main = initializeAll >> do
  let winW = 700 :: Int32; winH = 700 :: Int32
  engineMoveQueue <- atomically newTQueue   
  userMoveQueue <- atomically newTQueue   
  boardVar <- atomically $ newTVar [newBoardOf Nothing]
  textureVar <- atomically $ newTVar $ error "Texture uninitialized"
  window <- createWindow "Bubogo" $ defaultWindow
    { windowInitialSize = V2 (fromIntegral winW) (fromIntegral winH)
    , windowPosition = Absolute $ P $ V2 0 700 -- FIXME just for dev
    , windowResizable = True}
  renderer <- createRenderer window (-1) $ defaultRenderer
      --{rendererType = AcceleratedVSyncRenderer} -- black screen..
      {rendererType = UnacceleratedRenderer}
  st <- genWindowContent $ AppState winW winH 0 0 0
    engineMoveQueue userMoveQueue renderer textureVar
    boardVar Nothing
  texture <- atomically $ readTVar textureVar
  copy renderer texture Nothing Nothing
  present renderer
  let go e moves = do
        userMoves <- atomically $ readTQueue userMoveQueue
        let vanishRedo :: IOError -> IO (Engine, Maybe Move)
            vanishRedo exc = if ioe_type exc `elem` [EOF, ResourceVanished]
              then do
                slog "Engine vanished. Restarting..."
                e2 <- startEngine
                slog "Done."
                slog "Replaying moves.."
                mapM_ (ePlayMove e2) $ reverse $ concat moves
                slog "Done."
                tryWithE e2
              else throwIO exc
            tryWithE e2 = do
              case userMoves of
                [] -> do
                  slog "Doing undo with engine.."
                  ePut e "undo"
                  slog "Done."
                  go e (drop 1 moves)
                _ -> mapM_ (ePlayMove e2) userMoves
              (,) e2 <$> eGenMove e2 White
        (e3, engineMove) <- handle vanishRedo (tryWithE e)
        atomically $ writeTQueue engineMoveQueue engineMove
        --computeScore boardVar
        go e3 ((maybeToList engineMove):userMoves:moves)
  _ <- forkIO $ startEngine >>= \e -> go e []
  appLoop st
posToCell :: AppState -> V2 Int32 -> Maybe (V2 Int32)
posToCell st (V2 x y) =
  if cellX >= 0 && cellX <= 18 && cellY >= 0 && cellY <= 18
  then Just (V2 cellX cellY) else Nothing
  where
  cellX = (x - sBoardLeftX st) `div` sCellSize st
  cellY = (y - sBoardTopY st)  `div` sCellSize st
evNeedsPresent :: Event -> Bool
evNeedsPresent event = case eventPayload event of
  WindowExposedEvent _ -> True; WindowMovedEvent _ -> True;
  WindowShownEvent _ -> True; _ -> False
evToClick :: Event -> [V2 Int32]
evToClick e = case eventPayload e of
  MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ (P v)) -> [v]
  _ -> []
evsToNeedsPresentAndClicks :: [Event] -> (Bool, [V2 Int32])
evsToNeedsPresentAndClicks [] = (False, [])
evsToNeedsPresentAndClicks (e:es) =
  (evNeedsPresent e || p, evToClick e ++ cs)
  where (p, cs) = evsToNeedsPresentAndClicks es
appProcEvs :: AppState -> [Event] -> IO ()
appProcEvs st@AppState{sBoardVar=boardVar,sRenderer=renderer,
    sTexture=textureVar} es = do
  let (presDue,cs) = evsToNeedsPresentAndClicks es
  (st2,rendDue,presDue2) <- case catMaybes $ map (posToCell st) cs of
    V2 x y:_ -> do
        print (x, y) -- debug
        boards@(board:prevBoards) <- atomically $ readTVar boardVar
        case bRead board (Coord y x) of
          Nothing -> do
            let userMove = Move Black $ Coord y x
                userMoves = if null prevBoards
                  then
                    [ Move Black $ Coord 3 15
                    , Move Black $ Coord 15 3
                    , Move Black $ Coord 3 3
                    , Move Black $ Coord 15 15
                    ]
                  else [userMove]
            atomically $ writeTQueue (sUserMoveQueue st) userMoves
            board2 <- bPlayMoves board userMoves
            atomically $ writeTVar boardVar (board2:boards)
            _ <- genWindowContent $ st {sRecent = Just $ Coord y x}
            texture <- atomically $ readTVar textureVar
            copy renderer texture Nothing Nothing
            present renderer
            Just engineMove <- atomically $ readTQueue (sEngineMoveQueue st)
            let engineCoordMb = case engineMove of
                  Move _ engineCoord -> Just engineCoord
                  _ -> Nothing
            board3 <- bPlayMoves board2 [engineMove]
            atomically $ writeTVar boardVar (board3:boards)
            return (st {sRecent = engineCoordMb}, True, True)
          _ -> return (st, False, presDue)
    _ -> return (st, False, presDue)
  when presDue2 $ present renderer
  appLoop st2
  {-
      procResize event = case eventPayload event of
        WindowResizedEvent (WindowResizedEventData _ (V2 x y)) ->
          Just (toI x, toI y)
        _ -> Nothing
  let (st3, renderDue2, presentDue2) =
        case catMaybes $ map procResize events of
          (w,h):_ -> (st2 {sWinW = w, sWinH = h}, True, True)
          _ -> (st2, renderDue, presentDue)
  (renderDue3, presentDue3) <- if any isUPress events
    then do
      boards <- atomically $ readTVar boardVar
      case boards of
        _:(prevBoards@(_:_:_)) -> do
          atomically $ writeTVar boardVar prevBoards
          atomically $ writeTQueue (sUserMoveQueue st) []
          atomically $ writeTQueue (sUserMoveQueue st) []
          return (True, True)
        _ -> return (False, False)
    else return (renderDue2, presentDue2)
  st4 <- if renderDue3
    then do
      st4 <- genWindowContent st3
      texture2 <- atomically $ readTVar textureVar
      copy renderer texture2 Nothing Nothing
      return st4
    else return st3
  when presentDue3 $ present renderer
  appLoop st4
  -}
appLoop :: AppState -> IO ()
appLoop st = liftM2 (:) waitEvent pollEvents >>= appProcEvs st
