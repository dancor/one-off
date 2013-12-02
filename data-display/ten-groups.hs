import Data.List
import Data.List.Split

main :: IO ()
main =
    interact (unlines . zipWith (\n l ->
    show (10 * n) ++ "+) " ++ intercalate " " l) [0..] . chunksOf 10 . lines)
