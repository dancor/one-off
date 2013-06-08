#include <h>



{-
import qualified Data.MemoCombinators as Memo

fib1 = Memo.integral fib1'
  where
    fib1' 0 = 0
    fib1' 1 = 1
    fib1' n = trace ("Called on " ++ show n) $ fib1 (n-1) + fib1 (n-2)

fib :: Int -> Int -> (Int, Int)
fib = Memo.integral fib'
  where
    fib' 0 = \a -> (0, a)
    fib' 1 = \a -> (1, a)
    fib' n = trace ("Called on " ++ show n) $ \a ->
        (fst (fib (n-1) 0) + fst (fib (n-2) 0), a)

main = do
    print $ fib1 30
    print $ fib 30 0
-}

f :: BS.ByteString -> Int
f x = case x of
    " " -> 4

main = do
    print 4
