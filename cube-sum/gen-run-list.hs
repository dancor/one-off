import Text.Printf

f :: Int -> IO ()
f n = printf "./v1 %07d > out/cur/%07d 2>> stats.txt\n" n n

main :: IO ()
main = mapM_ f [0700000 .. 0899999]
