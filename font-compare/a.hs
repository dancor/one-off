#include <h>

showBinary :: Int -> String
showBinary x = showIntAtBase 2 intToDigit x ""

hexToInt :: String -> Int
hexToInt = fst . head . readHex

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

hexStrToBits :: String -> [Int]
hexStrToBits = concatMap (hexToBits . hexToInt . (:[]))

onesToIndices :: [Int] -> [Int]
onesToIndices = map fst . filter ((== 1) . snd) . zip [0..]

fontOrds :: String -> IO (Set.Set Int)
fontOrds a = do
  out <- lines <$> readProcess "fc-query" [a] ""
  return $
    Set.fromList $
    concatMap (\ (a, b) -> map (32 * 8 * a +) $ onesToIndices b) $
    map (\ x ->
      (hexToInt $ take 4 x,
      concatMap (reverse . hexStrToBits) $ words $ drop 6 x)) $
    map (drop 1) $
    filter (all (`elem` " \t:0123456789abcdef")) out

main = do
  [f1, f2] <- getArgs
  r1 <- fontOrds f1
  r2 <- fontOrds f2
  let
    r1Only = Set.difference r1 r2
    r2Only = Set.difference r2 r1
  putStrLn $ "f1 has " ++ show (Set.size r1) ++ " chars"
  putStrLn $ "f2 has " ++ show (Set.size r2) ++ " chars"
  putStrLn $ "f1 has " ++ show (Set.size r1Only) ++ " chars not in f2"
  putStrLn $ "f2 has " ++ show (Set.size r2Only) ++ " chars not in f1"
  putStr $ unlines $ map show $ Set.toList r1Only
