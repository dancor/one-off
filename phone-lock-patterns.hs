import Control.Monad.Random
import Data.Function
import Data.List

randShuffle :: (MonadRandom m) => [b] -> m [b]
randShuffle l = do
  rndInts <- getRandoms
  return . map snd . sortBy (compare `on` fst) $ zip (rndInts :: [Int]) l

randChoice :: (MonadRandom m) => [b] -> m b
randChoice l = randShuffle l >>= return . head

dots = [(row, col) | row <- [0..2], col <- [0..2]]

line (r, c) (r', c') = takeWhile (/= (r', c')) $
  zip [r, r + (r' - r) `div` g ..] [c, c + (c' - c) `div` g ..]
    where g = gcd (r' - r) (c' - c)

extensions pattern@(dot : _) =
  [new : pattern | new <- dots,
   new `notElem` pattern, all (`elem` pattern) $ line dot new]

search pattern found = foldr search (pattern : found) $ extensions pattern

valid pattern = length pattern >= 4
valid4 pattern = length pattern == 4

-- main = print . length . filter valid4 . foldr search [] $ map return dots
main = do
    c <- randChoice . filter valid . foldr search [] $ map return dots
    print c
