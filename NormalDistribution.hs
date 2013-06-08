#include <h>

import Statistics.Distribution.Normal
import Statistics.Distribution

pVal :: Double -> Int -> Double -> Int -> Double
pVal p1 n1 p2 n2 = cumulative standard z
  where
    n1d = fromIntegral n1
    n2d = fromIntegral n2
    psp = (p1 * n1d + p2 * n2d) / (n1d + n2d)
    se = sqrt $ psp * (1 - psp) * (1 / n1d + 1 / n2d)
    z = (p1 - p2) / se

main = do
    [p1s, n1s, p2s, n2s] <- getArgs
    print $ pVal (read p1s) (read n1s) (read p2s) (read n2s)
