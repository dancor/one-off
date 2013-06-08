import Foreign.C

foreign import ccall "my_lib.h my_func"
    c_my_func :: CInt -> CInt

myFunc :: Int -> Int
myFunc = fromIntegral . c_my_func . fromIntegral

main = do
  print $ myFunc 4
