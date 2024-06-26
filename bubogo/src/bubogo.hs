{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import GHC.Int (Int32)
import GHC.IO.Exception
import "cairo-canvas" Graphics.Rendering.Cairo.Canvas hiding (toD)
import "sdl2-cairo" SDL.Cairo
import "sdl2" SDL
import Board
import Color
import Coord
import Engine
import Move
bgColor, boardColor, lineColor, whiteColor, blackColor, recentColor :: V4 Byte
--bgColor     = V4 0xcc 0xff 0xcc 0xff
bgColor     = V4 0x00 0x00 0x00 0xff
boardColor  = V4 0xee 0xee 0x00 0xff
lineColor   = V4 0x00 0x00 0x00 0xff
whiteColor  = V4 0xff 0xff 0xff 0xff
blackColor  = V4 0x44 0x44 0x44 0xff
recentColor = V4 0xff 0x88 0x88 0xff
toD :: Integral a => a -> Double
toD = fromIntegral
data AppState = AppState {
  sWinW            :: !Int32,
  sWinH            :: !Int32,
  sBoardLeftX      :: !Int32,
  sBoardTopY       :: !Int32,
  sCellSize        :: !Int32,
  sFromE           :: !(TQueue (Maybe Move)),
  sToE             :: !(TQueue [Maybe Move]),
  sRenderer        :: !Renderer,
  sTexture         :: !(TVar Texture),
  sBoardVar        :: !(TVar [Board]),
  sRecent          :: !(Maybe Coord)}
genWindowContent :: AppState -> IO AppState
genWindowContent st@AppState{sWinW=winW,sWinH=winH,sTexture=textureVar} = do
  bd:_ <- atomically $ readTVar $ sBoardVar st
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
    sequence_ [sequence_ [when (bRead bd (Coord y x) == Just Black) $
        circle (cellXY x y) (toD cellSize)| x <- [0..18]] | y <- [0..18]]
    fill whiteColor
    sequence_ [sequence_ [when (bRead bd (Coord y x) == Just White) $
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
  rend <- createRenderer window (-1) $ defaultRenderer
    --{rendererType = AcceleratedVSyncRenderer} -- black screen..
    {rendererType = UnacceleratedRenderer}
  st <- genWindowContent $ AppState winW winH 0 0 0
    engineMoveQueue userMoveQueue rend textureVar
    boardVar Nothing
  texture <- atomically $ readTVar textureVar
  copy rend texture Nothing Nothing
  present rend
  let go e moves = do
        userMoves <- atomically $ readTQueue userMoveQueue
        let vanishRedo :: IOError -> IO (Engine, Maybe Move)
            vanishRedo exc = if ioe_type exc `elem` [EOF, ResourceVanished]
              then do
                e2 <- slog "Engine vanished. Restarting..." >> startEngine
                slog "Done. Replaying moves.."
                mapM_ (ePlayMove e2) $ reverse $ concat moves
                slog "Done." >> tryWithE e2
              else throwIO exc
            tryWithE e2 = do
              case userMoves of
                [] -> do
                  slog "Doing undo with engine.."
                  ePut e2 "undo"
                  slog "Done."
                  go e2 (drop 1 moves)
                [Nothing] -> slog "score" >> eScore e2 >> go e2 moves
                _ -> mapM_ (ePlayMove e2 . fromJust) userMoves
              (,) e2 <$> eGenMove e2 White
        (e3, engineMove) <- handle vanishRedo (tryWithE e)
        atomically $ writeTQueue engineMoveQueue engineMove
        go e3 ((maybeToList engineMove):map fromJust userMoves:moves)
  _ <- forkIO $ startEngine >>= \e -> go e []
  appLoop st
posToCell :: AppState -> V2 Int32 -> Maybe (V2 Int32)
posToCell st (V2 x y) =
  if cellX >= 0 && cellX <= 18 && cellY >= 0 && cellY <= 18
  then Just (V2 cellX cellY) else Nothing where
  cellX = (x - sBoardLeftX st) `div` sCellSize st
  cellY = (y - sBoardTopY st)  `div` sCellSize st
data EAcc = EAcc
  { ePresDue :: !Bool
  , eClick   :: !(Maybe (V2 Int32))
  , eResize  :: !(Maybe (V2 Int32))
  , eKey     :: !(Maybe Char)}
accEs :: EAcc -> Event -> EAcc
accEs !a e = let p = a {ePresDue = True} in case eventPayload e of
  WindowExposedEvent _ -> p; WindowMovedEvent _ -> p; WindowShownEvent _ -> p
  WindowResizedEvent (WindowResizedEventData _ v) -> a {eResize = Just v}
  MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ (P v)) -> 
    a {eClick = Just v}
  KeyboardEvent keyboardEvent ->
    if keyboardEventKeyMotion keyboardEvent == Pressed
      then case keysymKeycode (keyboardEventKeysym keyboardEvent) of
        KeycodeU -> a {eKey = Just 'u'}
        KeycodeF -> a {eKey = Just 'f'}
        _ -> a
      else a
  _ -> a
appProcEvs :: AppState -> [Event] -> IO ()
appProcEvs st@AppState{sBoardVar=bV,sRenderer=rend, sTexture=textureVar} es =
  let eAcc = foldl' accEs (EAcc False Nothing Nothing Nothing) es in do
  (st2,didClick) <- case posToCell st =<< eClick eAcc of
    Just (V2 x y) -> do
      bds@(bd:prevBds) <- atomically $ readTVar bV
      case bRead bd (Coord y x) of
        Nothing -> do
          let userMove = Move Black $ Coord y x
              userMoves = if null prevBds then
                [ Move Black $ Coord 3 15
                , Move Black $ Coord 15 3
                , Move Black $ Coord 3 3
                , Move Black $ Coord 15 15
                , Move Black $ Coord 9 3
                , Move Black $ Coord 9 15
                ] else [userMove]
          atomically $ writeTQueue (sToE st) $ map Just userMoves
          board2 <- bPlayMoves bd userMoves
          atomically $ writeTVar bV (board2:bds)
          _ <- genWindowContent $ st {sRecent = Just $ Coord y x}
          texture <- atomically $ readTVar textureVar
          copy rend texture Nothing Nothing
          present rend
          Just engineMove <- atomically $ readTQueue (sFromE st)
          board3 <- bPlayMoves board2 [engineMove]
          atomically $ writeTVar bV (board3:bds)
          let recent = case engineMove of Move _ a -> Just a; _ -> Nothing
          return (st {sRecent = recent}, True)
        _ -> return (st, False)
    _ -> return (st, False)
  let (st3,didResize) = case eResize eAcc of
        Just (V2 w h) -> (st2 {sWinW = w, sWinH = h}, True)
        _ -> (st2, False)
  didUndo <- case eKey eAcc of
    Just 'u' -> do
      bds <- atomically $ readTVar bV
      case bds of
        _:(prevBds@(_:_:_)) -> do
          atomically $ writeTVar bV prevBds
          atomically $ writeTQueue (sToE st) []
          atomically $ writeTQueue (sToE st) []
          slog "Did undo in game tree."
          return True
        _ -> slog "Nothing to undo in game tree." >> return False
    Just 'f' -> do
      atomically $ writeTQueue (sToE st) [Nothing]
      return False
    _ -> return False
  (st4,didRend) <- if didClick||didResize||didUndo then do
      st4 <- genWindowContent st3
      texture2 <- atomically $ readTVar textureVar
      copy rend texture2 Nothing Nothing
      return (st4, True)
    else return (st3, False)
  when (ePresDue eAcc || didRend) (present rend) >> appLoop st4
appLoop :: AppState -> IO ()
appLoop st = liftM2 (:) waitEvent pollEvents >>= appProcEvs st
