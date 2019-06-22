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
import System.IO
import System.Process
import qualified Text.ParserCombinators.Parsec as Psec

import Board
import Color
import Coord
import Engine
import Move

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

main :: IO ()
main = do
    playEngine
