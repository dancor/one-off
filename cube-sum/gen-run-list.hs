import Text.Printf

f :: Int -> IO ()
-- f n = printf "./cube-sum-no-cache %07d > out/%07d 2>> stats.txt\n" n n
f n = printf "./cube-sum %07d > out/%07d 2>> stats.txt\n" n n

-- main = mapM_ f (reverse [1..70499])
main = mapM_ f [15212 .. 70432]
