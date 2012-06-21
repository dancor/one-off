#include <h>

upToMOfARestB :: Int -> Int -> a -> a -> [[a]]
upToMOfARestB n m a b
 | m < 0 = []
 | m == 0 = [replicate n b]
 | n == 0 = [[]]
 | otherwise =
  map (a:) (upToMOfARestB (n - 1) (m - 1) a b) ++ 
  map (b:) (upToMOfARestB (n - 1) m a b)

-- equiv. class (rot./refl./inv) reps for n bits in a circle
allECReps :: Int -> [[Int]]
allECReps n =
  map head . group . sort . map toECRep $ upToMOfARestB n (n `div` 2) 1 0

invBit :: Int -> Int
invBit 0 = 1
invBit 1 = 0

-- 1) make less <= half lights on
-- 2) get binary number as "low" as possible; put largest 0's-seq first etc.
toECRep :: [Int] -> [Int]
toECRep bits = doRotRefl (doInv bits) where
  n = length bits
  doInv bs = if 2 * sum bs > n then map invBit bs else bs
  doRotRefl bs = minimum $ finiteCycles n bs ++ finiteCycles n (reverse bs)

finiteCycles :: Int -> [Int] -> [[Int]]
finiteCycles n = take n . map (take n) . tails . cycle

toRotOnlyECRep :: [Int] -> [Int]
toRotOnlyECRep bits = minimum $ finiteCycles (length bits) bits

hasFlipSym :: [Int] -> Bool
hasFlipSym bs = toRotOnlyECRep bs == toRotOnlyECRep (reverse bs)

doAtPos :: Int -> (a -> a) -> [a] -> [a]
doAtPos i f (x:xs)
 | i < 0 = error "i < 0"
 | i == 0 = f x : xs
 | otherwise = x : doAtPos (i - 1) f xs
doAtPos _ _ [] = []

oneOffFlipSym :: Bool -> [Int] -> Bool
oneOffFlipSym ignAlreadySym bs = 
  (ignAlreadySym || odd n) && hasFlipSym bs ||
  any (\ i -> hasFlipSym $ doAtPos i invBit bs) [0 .. n - 1]
  where
  n = length bs

showcaseAsyms :: IO ()
showcaseAsyms = do
  putStrLn "These are not exactly one bit-flip away from reflection symmetry:"
  putStr . unlines . map show . filter (not . null) $ map (map fst . 
    filter (not . snd) . map (\ x -> (x, oneOffFlipSym False x)) . 
    allECReps) [1..12]
  putStrLn "For 12 and up the number of them grow quickly:"
  putStrLn "Ignoring already-symmetric ones:"
  print $ map (length . filter (not . oneOffFlipSym True) . allECReps) [1..20]
  putStrLn "Not ignoring already-symmetric ones:"
  print $ map (length . filter (not . oneOffFlipSym False) . allECReps) [1..20]

main :: IO ()
main = showcaseAsyms
