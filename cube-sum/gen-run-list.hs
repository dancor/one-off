import Text.Printf

f :: Int -> IO ()
f n = printf "./cube-sum %07d > out/%07d 2>> stats.txt\n" n n

main :: IO ()
main = mapM_ f [0215819 .. 1000000]
