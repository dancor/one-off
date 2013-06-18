import Control.Applicative
import Control.Arrow
import Data.Char
import Data.Ratio
import System.IO
import qualified Data.Map as M

main :: IO ()
main = do
  lenToVowToWds <- 
    M.map (catMapOn vowelCount) .
    catMapOn length . 
    lines <$> readFile "/usr/share/dict/scrabble"
  let 
    smallOnes = M.filterWithKey (\ k _ -> k <= 5) lenToVowToWds
    vowHeavy = M.mapWithKey 
      (\ n -> M.filterWithKey (\ v _ -> v % n >= 3 % 4)) 
      -- (\ n -> M.filterWithKey (\ v _ -> v % n > 3 % 4)) 
      smallOnes
  putStr . unlines . concatMap snd . take 15 . M.toList $ 
    M.map (concatMap snd . take 2 . M.toList) vowHeavy

catMapOn :: (Ord b) => (a -> b) -> [a] -> M.Map b [a] 
catMapOn f = M.fromListWith (++) . map (\ x -> (f x, [x]))

vowelCount :: String -> Int
vowelCount = length . filter (`elem` "AEIOU")

