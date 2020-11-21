#include <h>
import Share

readStart :: T -> Ts -> [Ts]
readStart _ []     = []
readStart l (x:xs) = case procTitle x of
  Just t -> readFromTitle l t xs
  _ -> readStart l xs

readFromTitle :: T -> T -> Ts -> [Ts]
readFromTitle _ t []     = []
readFromTitle l t (x:xs) = case procTitle x of
  Just t -> readFromTitle l t xs
  _ -> if x == "==" <> l <> "=="
    then readFromSection l [] t xs
    else readFromTitle l t xs

readFromSection :: T -> Ts -> T -> Ts -> [Ts]
readFromSection _ _   _    []     = error "End of input in section"
readFromSection l acc acc1 (x:xs) = if isH2 x
  then (acc ++ [acc1]) : readStart l xs
  else if shaPre `TL.isPrefixOf` x
    then (acc ++ [TL.dropEnd (TL.length textPost) acc1]) : readStart l xs
    else readFromSection l (acc ++ [acc1]) x xs

main :: IO ()
main = do
    [l] <- getArgs
    pages <- readStart (TL.pack l) . TL.lines <$> TL.getContents
    mapM_ (\(t:xs) -> putStrLn $ show t <> show (TL.unlines xs)) pages
