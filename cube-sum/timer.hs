import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import System.Process

-- doRun :: Int -> IO 

oneRun :: Int -> Int -> IO Double
oneRun progN arg = do
    (_, _, errStr) <- readProcessWithExitCode "sh"
        ["-c", "./v" ++ show progN ++ " " ++ show arg ++ " > o" ++ show progN]
        ""
    return $ read $ init $ words errStr !! 1

compRun :: [Int] -> [Int] -> IO [Double]
compRun progNs args = do
    timeLine <- sequence [oneRun progN arg | arg <- args, progN <- progNs]
    let timeMatrix = transpose $ chunksOf (length progNs) timeLine
    return $ map sum timeMatrix

main = do
    --system "make v1 && make v2"

    rs <- compRun [1, 2] [0199997 .. 0199998]

    mapM_ print $ map (/ (minimum rs)) rs
    system "diff o1 o2"
