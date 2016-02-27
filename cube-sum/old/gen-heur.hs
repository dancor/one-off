import Data.Array.Unboxed
import Data.Array.Base
import Data.Array.ST

import Data.Bits
import Data.Word

import GHC.Base
import GHC.Integer
import GHC.Integer.GMP.Internals

-- not very discriminating, but cheap, so it's an overall gain
cr512 :: UArray Int Bool
cr512 = runSTUArray $ do
    ar <- newArray (0,511) True
    let note s i
            | i < 512   = unsafeWrite ar i False >> note s (i+s)
            | otherwise = return ()
    note 4 2
    note 8 4
    note 32 16
    note 64 32
    note 256 128
    unsafeWrite ar 256 False
    return ar

-- Remainders modulo @3^3 * 31@
cubeRes837 :: UArray Int Bool
cubeRes837 = runSTUArray $ do
    ar <- newArray (0,836) False
    let note 837 = return ar
        note k = unsafeWrite ar ((k*k*k) `rem` 837) True >> note (k+1)
    note 0

-- Remainders modulo @7^2 * 13@
cubeRes637 :: UArray Int Bool
cubeRes637 = runSTUArray $ do
    ar <- newArray (0,636) False
    let note 637 = return ar
        note k = unsafeWrite ar ((k*k*k) `rem` 637) True >> note (k+1)
    note 0

-- Remainders modulo @19 * 37@
cubeRes703 :: UArray Int Bool
cubeRes703 = runSTUArray $ do
    ar <- newArray (0,702) False
    let note 703 = return ar
        note k = unsafeWrite ar ((k*k*k) `rem` 703) True >> note (k+1)
    note 0

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

main :: IO ()
main = do
    "{" ++
    intercalate "," [show . boolToInt $ unsafeAt cr512 n | n <- [0..511]] ++
    "}"
