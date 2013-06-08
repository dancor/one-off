{- When I run this, it makes no sound for 4s and then seg. faults.
 - gdb says: 
 - Program received signal SIGSEGV, Segmentation fault.
 - 0x00000000005af611 in freeHaskellFunctionPtr ()
 -}

import Control.Concurrent
import Data.Int
import Data.List
import Foreign.Marshal.Array
import Sound.PortAudio
import qualified Data.Vector.Storable as DVS

doMyStream :: Stream Int32 Int32 -> IO (Either Error ())
doMyStream s = do
    threadDelay 4000000
    return $ Right ()

sampleRate :: Int
sampleRate = 44100

framesPerBuffer :: Int
framesPerBuffer = 256

myVector :: DVS.Vector Int32
-- A very simple sawtooth tone at around 44100 / 256 = ~200 Hz, so about low-A.
myVector = DVS.generate framesPerBuffer fromIntegral

myCallback :: StreamCallback Int32 Int32
myCallback timeInfo statusFlags _inpN _inpPtr outPtr = do
    pokeArray outPtr (DVS.toList myVector)
    return Continue

main :: IO ()
main = do
    paErrMb <- withPortAudio $ withDefaultStream 0 2 (fromIntegral sampleRate)
        (Just framesPerBuffer) (Just myCallback) Nothing doMyStream
    print paErrMb
