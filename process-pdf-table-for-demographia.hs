#include <h>

import Control.Monad.Catana
import Data.List.Split

readPage :: Int -> Catana String [String] () ()
readPage n = do
  cg1 <- transpose <$> replicateM 4 (replicateM n consume)
  c56 <- splitEvery 2 <$> replicateM (2 * n) consume
  cg2 <- transpose <$> replicateM 2 (replicateM n consume)
  c910 <- splitEvery 2 <$> replicateM (2 * n) consume
  cg3 <- transpose <$> replicateM 3 (replicateM n consume)
  mapM_ produce $ zipWith5 (\ a b c d e -> a ++ b ++ c ++ d ++ e)
    cg1 c56 cg2 c910 cg3
  _ <- consume
  return ()

readAll = do
  readPage 61
  replicateM_ 10 $ readPage 71

main = do
  c <- readFile "/home/danl/wua-data"
  putStr . unlines . map (intercalate "\t") . execCatana readAll $ lines c
