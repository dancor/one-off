import FUtil

daysInMonths :: Bool -> [(Int, Int)]
daysInMonths l = zip [1..]
  [31, if l then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

days :: Bool -> [(Int, Int)]
days = concatMap (\ (m, d) -> map ((,) m) [1 .. d]) . daysInMonths

showDay :: Int -> Int -> String
showDay m d = show2 m ++ "/" ++ show2 d
  where show2 = padl '0' 2 . show

main = do
  putStr . unlines . map (uncurry showDay) $ days False
