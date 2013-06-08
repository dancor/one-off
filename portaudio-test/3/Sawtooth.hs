{- This program plays a simple sawtooth wave.  It is my minimal case for a
 - strange behavior I'm seeing.  "runghc" runs this program correctly.
 - "ghc --make Sawtooth && ./Sawtooth" also plays the sound correctly, but
 - then seg. faults; from "gdb ./Sawtooth":
 -     Program received signal SIGSEGV, Segmentation fault.
 -     0x00000000005c7af1 in freeHaskellFunctionPtr ()
 -
 - I also tried compiling with "-threaded", but get the same result.
 -
 - Note that I also see a "SIGBUS, Bus error" (and no sound) iff compiled for:
 - https://github.com/sw17ch/portaudio/blob/master/examples/Example1.hs
 - (or a segfault if I use "-threaded"; but still no sound).
 -
 - Versions:
 - - linux: 3.2.0-25-generic #40-Ubuntu SMP x86_64
 - - ghc: The Glorious Glasgow Haskell Compilation System, version 7.4.1
 - - portaudio: portaudio19-dev - Portable audio I/O - development files
 -}

module Main where

-- I've tried adding various (threadDelay)s but it didn't help.
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import qualified Sound.PortAudio as PA
import Foreign.C.Types (CFloat)
import Foreign.Storable (pokeElemOff, Storable)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr)
import qualified Data.Vector as V

pokeVector :: Ptr CFloat -> V.Vector Float -> IO ()
pokeVector outPtr = pokeArray outPtr . map realToFrac . V.toList

playSawtooth :: IO (Either PA.Error ())
playSawtooth = do
    let buffL = 200
        myVector = V.fromList $ map fromIntegral [0 .. buffL - 1]
    ret <- PA.withDefaultStream 0 1 44100 (Just buffL) Nothing Nothing $ \s ->
        do
            PA.startStream s
            allocaBytes buffL $ \outPtr -> do
                outPtr' <- newForeignPtr_ outPtr
                pokeVector outPtr myVector
                replicateM_ 100 $ PA.writeStream s (fromIntegral buffL) outPtr'
            PA.stopStream s
            putStrLn "The segfault happens after this point."
            return $ Right ()
    putStrLn "The segfault happens before this point."
    return $ Right ()

main :: IO ()
main = do
    PA.withPortAudio playSawtooth
    return ()
