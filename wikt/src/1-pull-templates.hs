#include <h>
import Share

readStart :: Ts -> [Ts]
readStart [] = []
readStart (x:xs) = case procTemplate x of
  Just t -> readFromTitle t xs
  _ -> readStart xs

readFromTitle :: T -> Ts -> [Ts]
readFromTitle t [] = error "End of input after title before text"
readFromTitle t (x:xs) = case procText x of
  Just t2 -> readFromText [t] t2 xs
  _       -> readFromTitle t xs

readFromText :: Ts -> T -> Ts -> [Ts]
readFromText acc acc1 [] = error "End of input in section"
readFromText acc acc1 (x:xs) = if shaPre `TL.isPrefixOf` x
  then (acc ++ [TL.dropEnd (TL.length textPost) acc1]) : readStart xs
  else readFromText (acc ++ [acc1]) x xs

main :: IO ()
main = do
    pages <- readStart . TL.lines <$> TL.getContents
    mapM_ (\(t:xs) -> putStrLn $ show t <> show (TL.unlines xs))
        pages
