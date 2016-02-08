-- Calculate integer solutions to:
-- a^3 + b^3 = c^3 + d^3, a > b, a > c, c > d. Implies c > b.

import Data.Functor
import Data.Maybe
import Data.Word
import Math.NumberTheory.Powers.Cubes
import System.Environment

type N = Word64

-- Largest x where 2x^3 doesn't overflow.
cubeMax :: N
cubeMax = 2097151

givenA :: N -> [(N, N, N, N)]
givenA a = concatMap (givenAB a (a * a * a)) [1 .. a - 2]

givenAB :: N -> N -> N -> [(N, N, N, N)]
givenAB a a3 b = catMaybes $ map (givenABC a b a3b3) [cMin .. a - 1]
  where
    a3b3 = a3 + b * b * b
    cMin = ceiling $ (fromIntegral a3b3 / 2) ** (1 / 3)

givenABC :: N -> N -> N -> N -> Maybe (N, N, N, N)
givenABC a b a3b3 c = ((,,,) a b c) <$> exactCubeRoot (a3b3 - c * c * c)

readMb :: Read a => String -> Maybe a
readMb s = fmap fst . listToMaybe $ reads s

main :: IO ()
main = do
    args <- getArgs
    let usage = "cube-sum [integer-value-of-a]"
    a <- case args of
      [] -> return 12
      [nStr] -> case readMb nStr of
        Just n -> return n
        _ -> error usage
      _ -> error usage
    mapM_ print $ givenA a
