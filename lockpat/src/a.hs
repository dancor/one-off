-- List all lock screen patterns on an n*n grid w/ minLen numbers in them.
n = 3
minLen = 4
n2 = n * n
growFrom :: [Int] -> [[Int]]
growFrom [] = concatMap (growFrom . (:[])) [1..n2]
growFrom ar@(a:_rest) = let l = length ar in
  (if l >= minLen then (ar:) else id) $
  if l < n2 then concat [growFrom (b:ar) | b <- [1..n2], b `notElem` ar,
    all (\c -> c `elem` b:ar || not (isLine a c b)) [1..n2]] else []
isLine :: Int -> Int -> Int -> Bool
isLine a b c = 
  (ax <= bx && bx <= cx || ax >= bx && bx >= cx) &&
  (ay <= by && by <= cy || ay >= by && by >= cy) &&
  (by - ay) * (cx - bx) == (bx - ax) * (cy - by) where
  (ay,ax)=quotRem (a-1) n; (by,bx)=quotRem (b-1) n; (cy,cx)=quotRem (c-1) n
main = mapM_ (putStrLn . concatMap show . reverse) $ growFrom []
