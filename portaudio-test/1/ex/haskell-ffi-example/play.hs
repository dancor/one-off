module Main where

import Foreign

foreign import ccall "buzzlib.h buzz" my_buzz :: Int -> Ptr (Int) -> IO ()

main :: IO ()
main = do
    let second_argument_to_buzz_function = [ 1, 2, 3, 4, 5 ] :: [Int]
        first_argument_to_buzz_function = length second_argument_to_buzz_function

    -- Calling the C function
    withArray second_argument_to_buzz_function (my_buzz first_argument_to_buzz_function)

    -- Line above is equivalent to the following lines:
    ptr <- newArray second_argument_to_buzz_function
    my_buzz first_argument_to_buzz_function ptr
    free ptr

    putStrLn "Done"
