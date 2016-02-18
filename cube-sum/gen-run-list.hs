import Text.Printf

f :: Int -> IO ()
f n = printf "./cube-sum %07d > out/%07d 2>> stats.txt\n" n n

main = mapM_ f $ [50328 .. 70432] ++ [100000 .. 2097151]
