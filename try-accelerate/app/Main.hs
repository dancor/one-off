{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU
import Formatting
import Formatting.Clock
import System.Clock

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

mill, ten_mill, hundo_mill :: Int
mill       = 1000000
ten_mill   = 10000000
hundo_mill = 100000000

main :: IO ()
main = do
    --             CPU   GPU   winner (4-core CPU vs. GTX1060-6GB)
    -- mill        46ms  81ms CPUx2   (CPU wins even more for smaller samples)
    -- ten_mill   340ms 100ms GPUx3.4
    -- hundo_mill 4s    250ms GPUx16
    let xs = fromList (Z:.ten_mill) [0..]   :: Vector Float
    let ys = fromList (Z:.ten_mill) [1,3..] :: Vector Float
    t1 <- getTime Monotonic
    print $ CPU.run $ dotp (use xs) (use ys)
    t2 <- getTime Monotonic
    print $ GPU.run $ dotp (use xs) (use ys)
    t3 <- getTime Monotonic
    fprint ("CPU: " % timeSpecs % "\n") t1 t2
    fprint ("GPU: " % timeSpecs % "\n") t2 t3
