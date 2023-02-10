module Engine where
import Control.Monad
import System.IO
import System.Process
import qualified Text.ParserCombinators.Parsec as Psec
import Board
import Color
import Coord
import Move
slog :: String -> IO ()
slog = putStrLn
--slog = const $ return ()
data Engine = Engine {eInH  :: !Handle, eOutH :: !Handle, eErrH :: !Handle}
startEngine :: IO Engine
startEngine = do
  (Just inH, Just outH, Just errH, _engProc) <- createProcess $
    (proc "gtp-engine" [])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  hSetBuffering inH  NoBuffering >> hSetBuffering outH NoBuffering
  hSetBuffering errH NoBuffering
  let e = Engine inH outH errH
  ePut e "time_settings 0 1 1" >> return e
ePut :: Engine -> String -> IO ()
ePut e s = slog ("IN: " ++ s) >> hPutStrLn (eInH e) s
ePlayMove :: Engine -> Move -> IO ()
ePlayMove _ Pass = return ()
ePlayMove e (Move color (Coord column row)) = ePut e $
  "play " ++ colorLtr color ++ " " ++ showColumn column ++ showRow row
eSetBoard :: Engine -> Board -> IO ()
eSetBoard e b = ePut e "clear_board" >> forM_ allCoords $ \c ->
  case bRead b c of Just color -> ePlayMove e (Move color c); _ -> return ()
eSetMoves :: Engine -> [Move] -> IO ()
eSetMoves e moves = ePut e "clear_board" >> mapM_ (ePlayMove e) moves
eScore :: Engine -> IO ()
eScore e = do
  ePut e "final_score"
  hFlush (eInH e)
  l <- hGetLine (eOutH e)
  slog $ "OUT: " ++ l
  l2 <- hGetLine (eOutH e)
  slog $ "OUT: " ++ l2
eGenMove :: Engine -> Color -> IO (Maybe Move)
eGenMove e color = do
  ePut e $ "genmove " ++ colorLtr color
  hFlush (eInH e)
  mvCoordMb <- eAwaitMoves e color
  case mvCoordMb of
    Just [] -> error "eAwaitMove: got no moves"
    Just (mv:_) -> return $ Just mv
    Nothing -> return Nothing
eAwaitMoves :: Engine -> Color -> IO (Maybe [Move])
eAwaitMoves = eAwaitMovesAccum []
eAwaitMovesAccum :: [Coord] -> Engine -> Color -> IO (Maybe [Move])
eAwaitMovesAccum coords e color = do
  l <- hGetLine (eOutH e)
  slog ("OUT: " ++ l) >> case l of
    "= " -> eAwaitMovesAccum coords e color
    "= pass" -> do
        putStrLn "Engine passes."
        return $ Just [Pass]
    "= resign" -> do
        putStrLn "Engine resigns."
        return $ Just [Pass]
    '=':' ':s -> do
        case Psec.parse coordParser "" s of
          Right coord -> return $ Just [Move color coord]
          _ -> do
              putStrLn "Could not understand engine move."
              return Nothing
    n:':':' ':rest -> slog rest >> do
      let (coordComma:_gameEqN:_percent:_) = words rest
          Just coord = readCoord $ takeWhile (/= ',') coordComma
      if n >= '1' && n <= '9'
        then eAwaitMovesAccum (coords ++ [coord]) e color
        else eAwaitMovesAccum coords e color
    _ -> eAwaitMovesAccum coords e color
