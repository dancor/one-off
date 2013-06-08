module Main where

import Sound.PortAudio.Base
import Sound.PortAudio

import Control.Monad (foldM, foldM_, forM_)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Text.Printf

import Foreign.C.Types
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

import qualified Data.Vector as V

sampRate :: Double
sampRate = 44100

framesPerBuffer :: Int
framesPerBuffer = 600

tableSize :: Int
tableSize = 200

data SineTable = SineTable { sine :: V.Vector Float }
data Phases = Phases { leftPhase :: Int, rightPhase :: Int }

newTable :: Int -> SineTable
newTable sze = SineTable vec where
    intSze = fromInteger $ toInteger sze
    vec = V.fromList $ map (\i -> sin $ (i / intSze) * pi * 2) [0..(intSze - 1)]

sineTable :: SineTable
sineTable = newTable tableSize

poker :: (Storable a, Fractional a) => Ptr a -> (Int, Int) -> Int -> IO (Int, Int)
poker out (l, r) i = do
    pokeElemOff out (2 * i)      (realToFrac $ (V.!) (sine sineTable) l)
    pokeElemOff out (2 * i + 1)  (realToFrac $ (V.!) (sine sineTable) r)
    let newL = let x = l + 1 in (if x >= tableSize then (x - tableSize) else x)
    let newR = let x = r + 3 in (if x >= tableSize then (x - tableSize) else x)
    return (newL, newR)

withBlockingIO :: IO (Either Error ())
withBlockingIO = do
    let iterations  = 50
        numChannels = 2
    
    withDefaultStream 0 numChannels sampRate (Just framesPerBuffer) Nothing Nothing $ \strm -> do
        s2 <- startStream strm

        allocaBytes (framesPerBuffer * numChannels) $ \out -> do
            
            out' <- newForeignPtr_ out
            
            let runFunc (l, r) i = do
                (newL', newR') <- foldM (poker (out :: Ptr CFloat)) (l, r) [0..(fromIntegral $ framesPerBuffer - 1)]
                writeStream strm (fromIntegral framesPerBuffer) out'
                return (newL', newR')
                
            foldM_ runFunc (0,0) [0..iterations]
        
        s3 <- stopStream strm
        return $ Right ()

main = do
    putStrLn $ "PortAudio Test: output sine wave. SR = " ++ (show sampRate) ++ ", BufSize = " ++ (show $ framesPerBuffer)
    
    withPortAudio withBlockingIO
    return ()
