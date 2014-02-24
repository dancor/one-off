{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign

foreign import ccall "lol.h lol"
    lol :: IO ()

main :: IO ()
main = lol
