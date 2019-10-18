{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.LLVM.PTX     as GPU
import Data.List
import Formatting
import Formatting.Clock
import Prelude                            as P
import System.Clock

wWidth, wHeight :: Int
wWidth =  2560
wHeight = 1440
type World = Matrix Int

dotp :: Acc (Vector T) -> Acc (Vector T) -> Acc (Scalar T)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

mill, ten_mill, hundo_mill :: Int
mill       = 1000000
ten_mill   = 10000000
hundo_mill = 100000000

type T = Int8

main :: IO ()
main = do
    --             CPU   GPU   winner (4-core CPU vs. GTX1060-6GB)
    -- mill        46ms  81ms CPUx2   (CPU wins even more for smaller samples)
    -- ten_mill   340ms 100ms GPUx3.4
    -- hundo_mill 4s    250ms GPUx16
    let xs = fromList (Z:.ten_mill) (repeat 67)   :: Vector T
    let ys = fromList (Z:.ten_mill) (repeat 55) :: Vector T
    t1 <- getTime Monotonic
    print $
        foldl' (+) 0 (P.zipWith (*) (P.take ten_mill (repeat 67)) (repeat 55))
    t2 <- getTime Monotonic
    print $ CPU.run $ dotp (use xs) (use ys)
    t3 <- getTime Monotonic
    print $ GPU.run $ dotp (use xs) (use ys)
    t4 <- getTime Monotonic
    fprint ("CPU1: " % timeSpecs % "\n") t1 t2
    fprint ("CPUm: " % timeSpecs % "\n") t2 t3
    fprint ("GPU:  " % timeSpecs % "\n") t3 t4
