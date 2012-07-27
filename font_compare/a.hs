#include <h>

showBinary :: Int -> String
showBinary x = showIntAtBase 2 intToDigit x ""

hexDigToInt :: Char -> Int
hexDigToInt d = fst $ head $ readHex [d]

hexToBits 0 = [0,0,0,0]
hexToBits 1 = [0,0,0,1]
hexToBits 2 = [0,0,1,0]
hexToBits 3 = [0,0,1,1]
hexToBits 4 = [0,1,0,0]
hexToBits 5 = [0,1,0,1]
hexToBits 6 = [0,1,1,0]
hexToBits 7 = [0,1,1,1]
hexToBits 8 = [1,0,0,0]
hexToBits 9 = [1,0,0,1]
hexToBits 10 = [1,0,1,0]
hexToBits 11 = [1,0,1,1]
hexToBits 12 = [1,1,0,0]
hexToBits 13 = [1,1,0,1]
hexToBits 14 = [1,1,1,0]
hexToBits 15 = [1,1,1,1]

fontBits a = do
  out <- lines <$> readProcess "fc-query" [a] ""
  return $
    map (== 1) $
    concat $
    map hexToBits $
    map hexDigToInt $
    concat $ concat $ 
    map (drop 1) $ map words $ 
    filter (
      (\ x -> length x > 0 && 
      all (`elem` (['0'..'9'] ++ ['a'..'f'])) x) . takeWhile (/= ':') . 
        dropWhile isSpace)
    out

main = do
  [f1, f2] <- getArgs
  r1 <- fontBits f1
  r2 <- fontBits f2
  let
    r1Only = zipWith (\ a b -> a && not b) r1 r2
    r2Only = zipWith (\ a b -> b && not a) r1 r2
  putStrLn $ "f1 has " ++ show (length $ filter id r1) ++ " chars"
  putStrLn $ "f2 has " ++ show (length $ filter id r2) ++ " chars"
  putStrLn $ "f1 has " ++ show (length $ filter id r1Only) ++ " chars not in f2"
  putStrLn $ "f2 has " ++ show (length $ filter id r2Only) ++ " chars not in f1"
  putStr $ unlines $
    splitEvery 40 $ 
    map chr $ map fst $ filter snd $ zip [0..] r1Only
