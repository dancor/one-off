import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import System.Process

-- doRun :: Int -> IO 

oneRun :: String -> IO Double
oneRun c = do
    (_, _, errStr) <- readProcessWithExitCode "sh" ["-c", c] ""
    return $ read $ init $ words errStr !! 1

compRun :: Int -> [String] -> IO ()
compRun n cs = do
    timeLine <- mapM oneRun $ concat $ replicate n cs
    let csNum = length cs
        timeMatrix = transpose $ chunksOf csNum timeLine
        totTimes = map sum timeMatrix
    mapM_ print totTimes

main = do
    system "make v1 && make v2"
    compRun 10 ["./v1 9999 > o1", "./v2 9999 > o2"]
    system "diff o1 o2"
