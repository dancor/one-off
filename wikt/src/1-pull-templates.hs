#include <h>
import Share

readStart :: Ts -> [Ts]
readStart [] = []
readStart (x:xs) = if isTitle x then readFromTitle x xs else readStart xs

readFromTitle :: T -> Ts -> [Ts]
readFromTitle t [] = error "End of input after title before text"
readFromTitle t (x:xs) = if textPre `TL.isPrefixOf` x
  then readFromText [] (procText t) xs
  else readFromTitle t xs

readFromText :: Ts -> T -> Ts -> [Ts]
readFromText acc acc1 [] = error "End of input in section"
readFromText acc acc1 (x:xs) = if shaPre `TL.isPrefixOf` x
  then (acc ++ [TL.drop (TL.length textPost) acc1]) : readStart xs
  else readFromText (acc ++ [acc1]) x xs

main :: IO ()
main = do
    pages <- readStart . TL.lines <$> TL.getContents
    mapM_ (\(t:xs) -> putStrLn $ show t <> show (TL.unlines xs)) pages
