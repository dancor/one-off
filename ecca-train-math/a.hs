import Data.List
import Data.Maybe
import Control.Arrow

f 0 = Nothing
f 230 = Just "h"
f 270 = Just "g"
f 425 = Just "e/f"
f 485 = Just "a"
f 455 = Just "b"
f 380 = Just "c/d"

main = putStr . unlines . map (\ (a, b) -> 
    "$" ++ take 2 (show a) ++ "." ++ drop 2 (show a) ++ ": " ++ b) . 
  map (second (intercalate " + " . 
    map (\ l -> (if length l == 1 then "" else show (length l) ++ " ") ++ 
      head l) . 
    group . catMaybes . sort . map f)) . sort . 
  filter ((\ x -> x > 1400 && x < 1500) . fst) . 
  map (\ a -> (sum a, a)) . nub . map sort . sequence $ 
  replicate 6 [485, 455, 380, 425, 270, 230, 0]
