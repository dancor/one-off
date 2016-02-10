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

compRun :: Int -> [String] -> IO [Double]
compRun n cs = do
    timeLine <- mapM oneRun $ concat $ replicate n cs
    let csNum = length cs
        timeMatrix = transpose $ chunksOf csNum timeLine
    return $ map sum timeMatrix

main = do
    system "make v1 && make v2"
    rs <- compRun 300 ["./v1 999 > o1", "./v2 999 > o2"]
    mapM_ print $ map (/ (minimum rs)) rs
    system "diff o1 o2"
