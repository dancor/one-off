import Text.Printf

f :: Int -> IO ()
f n = printf "./cube-sum %07d > out/cur/%07d 2>> stats.txt\n" n n

main :: IO ()
main = mapM_ f [0576531 .. 0899999]
