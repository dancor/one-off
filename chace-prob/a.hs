import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe

type Op = Maybe Int -> Maybe Int -> Maybe Int

nums :: [Int]
nums = [1..5]

intDiv :: Op
intDiv (Just a) (Just b) =
    if m == 0 then Just d else Nothing where (d, m) = divMod a b
intDiv _ _ = Nothing

justMul :: Op
justMul = liftA2 (*)

ops :: [(Op, Op)]
ops =
    [ (intDiv, justMul)
    , (justMul, intDiv)
    , (flip intDiv, justMul)
    ]

groups :: [Int] -> [[[Int]]]
groups ns = map f groupSizes
  where
    f (s1, s2) = [take s1 ns, take s2 $ drop s1 ns, drop s2 $ drop s1 ns]
    groupSizes = [(1, 1), (1, 2), (1, 3), (2, 1), (2, 2), (3, 1)]

listToNum :: [Int] -> Int
listToNum = foldl' (\ x y -> x * 10 + y) 0

doOps :: (Op, Op) -> [[Int]] -> Maybe Int
doOps (o1, o2) ns = o1 (Just a) $ o2 (Just b) (Just c) where
    [a, b, c] = map listToNum ns

main :: IO ()
main = do
    -- putStr $ unlines $ map show $ nub $ sort $ map snd $
    -- print $ length $ map show $ nub $ sort $ map snd $
    putStr $ unlines $ map show $ sortBy (compare `on` snd) $
        filter (isJust . snd)
        [ (ns, doOps o ns)
        | o <- ops, ns <- concatMap groups $ permutations nums
        ]
