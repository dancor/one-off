import Data.List.Utils
import Data.List.Split
import System.Process

readTime :: String -> Int
readTime s = 60 * (60 * a + b) + c
  where
    [a, b, c] = map read $ splitOn ":" s

main = do
    c <- readFile "aa"
    let (timeStrs, spNames) = unzip . map (break (== ' ')) $ lines c
        times = map readTime timeStrs
        names = map tail spNames
        offsets = map Just (zipWith (-) (tail times) times) ++ [Nothing]
    sequence_ $ zipWith3 (\ time offsetMb name ->
            rawSystem "avconv" $ ["-i", "b.webm", "-acodec", "copy", "-vn",
            "-ss", show time] ++
            (maybe [] (\offset -> ["-t", show offset]) offsetMb) ++
            [replace " " "-" name ++ ".ogg"]
        ) times offsets names
