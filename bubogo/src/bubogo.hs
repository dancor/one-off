{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleInstances #-}

-- The higher the digit the better for the player about to play.
-- 9 means 90+% estimated chance of winning.

-- Engine bugs in AQ that we work around (I am using AQ v2.0.2),
-- but our workarounds would still work with correct engines:
-- - genmove [color]: it always ignores color and plays who it thinks is next
-- - play [color] [square]: if color is white it ignores color and plays
--   who is next
-- - use "play pass" to get around these issues

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Text.ParserCombinators.Parsec as Psec
import System.IO
import System.Process

import Board
import Color
import Coord
import Engine
import Move

data Engine = Engine
  { eInH  :: !Handle
  , eOutH :: !Handle
  , eErrH :: !Handle
  }

getEval :: Handle -> IO String
getEval engErr = do
    l <- hGetLine engErr
    if "1: " `isPrefixOf` l
      then return $ takeWhile (/= '[') (words l !! 3)
      else do
        getEval engErr

ePut :: Engine -> String -> IO ()
ePut e s = do
    slog $ "IN: " ++ s
    hPutStrLn (eInH e) s

ePlayMove :: Engine -> Move -> IO ()
ePlayMove e (Move color (Coord column row)) = do
    ePut e $
        "play " ++ colorLtr color ++ " " ++ columnStr column ++ rowStr row

eSetBoard :: Engine -> Board -> IO ()
eSetBoard e b = do
    ePut e "clear_board"
    forM_ allCoords $ \c -> do
        case bRead b c of
          Just color -> ePlayMove e (Move color c)
          _ -> return ()

eSetMoves :: Engine -> [Move] -> IO ()
eSetMoves e moves = ePut e "clear_board" >> mapM_ (ePlayMove e) moves

eGenMove :: Engine -> Color -> IO ()
eGenMove e color = do
    --when (color == White) $ ePut e "play pass"
    ePut e $ "genmove " ++ colorLtr color
    --when (color == Black) $ ePut e "play pass"
    hFlush (eInH e)

data GetMove
    = GotQuit
    | EngineMove Color
    | EngineGame Color
    | Got Move
    | GotUndo
    deriving Show

moveParser :: Color -> Psec.Parser GetMove
moveParser defColor = Psec.choice
  [ Psec.try $ Psec.string "undo" >> return GotUndo          <* Psec.eof
  , Psec.try $ Psec.string "u"    >> return GotUndo          <* Psec.eof
  , Psec.try $ Got <$> liftM2 Move colorParser coordParser   <* Psec.eof
  , Psec.try $ (Got . Move defColor) <$> coordParser         <* Psec.eof
  , Psec.try $ Psec.char 'e' >> EngineMove <$> colorParser   <* Psec.eof
  , Psec.try $ Psec.char 'e' >> return (EngineMove defColor) <* Psec.eof
  , Psec.try $ Psec.char 'E' >> return (EngineGame defColor) <* Psec.eof
  , Psec.try $ Psec.string "quit" >> return GotQuit          <* Psec.eof
  , Psec.try $ Psec.string "q"    >> return GotQuit          <* Psec.eof
  ,            Psec.string "exit" >> return GotQuit          <* Psec.eof
  ]

getMove :: Color -> IO [GetMove]
getMove defColor = do
    putStr $ "Your move (" ++ show defColor ++ "): "
    hFlush stdout
    moveStrs <- words <$> getLine
    case sequence (map (Psec.parse (moveParser defColor) "") moveStrs) of
      Right ret -> return ret
      _ -> do
        putStrLn "I could not read your move. Please try again (e.g. D4)."
        getMove defColor

eAwaitMove :: Engine -> Color -> IO (Maybe Move)
eAwaitMove e color = do
    mvCoordMb <- eAwaitMoves e color
    case mvCoordMb of
      Just [] -> error "eAwaitMove: got no moves"
      Just (mv:_) -> return $ Just mv
      Nothing -> return Nothing

eAwaitMoves :: Engine -> Color -> IO (Maybe [Move])
eAwaitMoves = eAwaitMovesAccum []

slog = putStrLn

eAwaitMovesAccum :: [Coord] -> Engine -> Color -> IO (Maybe [Move])
eAwaitMovesAccum coords e color = do
    l <- hGetLine (eOutH e)
    slog $ "OUT: " ++ l
    case l of
      "= " -> eAwaitMovesAccum coords e color
      "= resign" -> do
          putStrLn "Engine resigns."
          return Nothing
      '=':' ':s -> do
          case Psec.parse coordParser "" s of
            Right coord -> return $ Just [Move color coord]
            _ -> do
                putStrLn "Could not understand engine move."
                return Nothing
      n:':':' ':rest -> do
        putStrLn rest
        let (coordComma:gameEqN:percent:_) = words rest
            Just coord = readCoord $ takeWhile (/= ',') coordComma
        if n >= '1' && n <= '9'
          then eAwaitMovesAccum (coords ++ [coord]) e color
          else eAwaitMovesAccum coords e color
      _ -> eAwaitMovesAccum coords e color

mutBPlayMoves :: MBoard -> [Move] -> IO ()
mutBPlayMoves b = mapM_ (bPlayMove b)

bPlayMoves :: Board -> [Move] -> IO Board
bPlayMoves b mvs = do
  mutB <- V.thaw b
  mutBPlayMoves mutB mvs
  V.freeze mutB

withEngine :: (Engine -> IO a) -> IO a
withEngine f = withCreateProcess (
    (proc "gtp-engine" [])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}) $
    -- \(Just inH) (Just outH) (Just errH) _engProc -> f (Engine inH outH errH)
    \(Just inH) (Just outH) (Just errH) _engProc -> do
        hSetBuffering inH NoBuffering
        hSetBuffering outH NoBuffering
        hSetBuffering errH NoBuffering
        f (Engine inH outH errH)

setTime :: Engine -> IO ()
setTime e = do
  ePut e "time_settings 0 1 1"
  return ()

playEngine :: IO ()
playEngine = withEngine $ \e -> setTime e >> go e [newBoardOf Nothing] [] where
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

-- It is assumed that the Board has Color at Coord.
getChain :: MBoard -> Color -> Coord -> IO (MBoardOf Bool)
getChain b color c = do
    chain <- newMBoardOf False
    growChain b chain color c
    return chain

growChain :: MBoard -> MBoardOf Bool -> Color -> Coord -> IO ()
growChain b chain color c = do
    alreadySeen <- mBRead chain c
    unless alreadySeen $ do
        v <- mBRead b c
        when (v == Just color) $ do
            mBWrite chain c True
            mapM_ (growChain b chain color) $ coordNeighbors c

orM :: [IO Bool] -> IO Bool
orM [] = return False
orM (x:xs) = do
    r <- x
    if r then return True else orM xs

anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM f xs = orM (map f xs)

chainCoords :: MBoardOf Bool -> IO [Coord]
chainCoords chain = catMaybes <$> mapM isIn allCoords
  where
    isIn c = do
        inChain <- mBRead chain c
        return $ if inChain then Just c else Nothing

chainHasLiberties :: MBoard -> MBoardOf Bool -> IO Bool
chainHasLiberties b chain = anyM cHasLib =<< chainCoords chain
  where cHasLib = anyM (fmap isNothing . mBRead b) . coordNeighbors

captureChain :: MBoard -> MBoardOf Bool -> IO ()
captureChain b chain = mapM_ (\c -> mBWrite b c Nothing) =<< chainCoords chain

tryCapture :: MBoard -> Color -> Coord -> IO ()
tryCapture b capturedColor c = do
    v <- mBRead b c
    when (v == Just capturedColor) $ do
        chain <- getChain b capturedColor c
        hasLib <- chainHasLiberties b chain
        unless hasLib $ captureChain b chain

bPlayMove :: MBoard -> Move -> IO ()
bPlayMove b (Move color c) = do
    mBWrite b c (Just color)
    mapM_ (tryCapture b (otherColor color)) $ coordNeighbors c

readSgfMoves :: String -> [Move]
readSgfMoves = catMaybes . map readSgfMoveLineMb . lines
  where
    readSgfLtr :: Char -> Maybe Int
    readSgfLtr ltr =
      if 'a' <= ltr && ltr <= 't'
        then Just (ord ltr - ord 'a')
        else Nothing
    readSgfMoveLineMb :: String -> Maybe Move
    readSgfMoveLineMb (';':cLtr:'[':columnLtr:rowLtr:']':_) = do
      c <- case cLtr of
        'B' -> Just Black
        'W' -> Just White
        _ -> Nothing
      row <- readSgfLtr rowLtr
      column <- readSgfLtr columnLtr
      return $ Move c (Coord column row)
    readSgfMoveLineMb _ = Nothing

main :: IO ()
main = do
    playEngine
