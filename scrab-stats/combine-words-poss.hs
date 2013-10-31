import Control.Monad
import qualified Data.MemoCombinators as Memo
import System.Environment

numWdsWithNChars :: Int -> Integer
numWdsWithNChars x = [0, 0, 0,
    -- Starting at (!! 3) for 3 letters:
    1292, 5454, 12478, 22157, 32909, 40161] !! x
    --1015, 4030, 8938, 22157, 32909, 40161] !! x

numPoss :: Int -> Int -> Integer
numPoss = Memo.memo2 Memo.integral Memo.integral f
  where
    f maxWdLen 0 = 1
    f maxWdLen pwCharLen =
        sum
        [ (numWdsWithNChars i) * numPoss maxWdLen (pwCharLen - i)
        | i <- [1 .. min maxWdLen pwCharLen]]

main = do
    [maxWdLenStr] <- getArgs
    forM_ [1 .. 30] $ \ len -> putStrLn $
        show len ++ "\t" ++
        show (log (fromIntegral $ numPoss (read maxWdLenStr) len) / log 2)
