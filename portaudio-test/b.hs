import Foreign
import Foreign.C.Types

foreign import ccall "math.h sin"
    c_sin :: CDouble -> CDouble

fastSin :: Double -> Double
fastSin = realToFrac . c_sin . realToFrac

main = do
  print $ fastSin 1.5
