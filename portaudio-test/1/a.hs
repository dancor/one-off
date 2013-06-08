{-# LANGUAGE CPP, ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

foreign import ccall "my_lib.h lol" c_lol :: CInt -> CInt

hsLol :: Int -> Int
hsLol = fromIntegral . c_lol . fromIntegral

main = do
    print $ hsLol 4
