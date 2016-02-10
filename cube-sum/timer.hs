import Control.Applicative
import Control.Monad
import System.Process

-- doRun :: Int -> IO 

oneRun :: String -> IO Double
oneRun c = do
    (_, _, errStr) <- readProcessWithExitCode "sh" ["-c", c] ""
    return $ read $ init $ words errStr !! 1

avgRun :: Int -> String -> IO Double
avgRun n c = (/ fromIntegral n) . sum <$> replicateM n (oneRun c)

main = do
    avgRun 10 "./v1 9999 > o1" >>= print
    avgRun 10 "./v2 9999 > o2" >>= print
    system "diff o1 o2"
