#include <h>
import Share
type Ts = [T]
type T = TL.Text

titlePre, titlePost, shaPre, textPost :: T
titlePre = "    <title>"
titlePost = "</title>"
shaPre = "      <sha1>"
textPost = "</text>"

isTitle, isH2 :: T -> Bool
isTitle = (titlePre `TL.isPrefixOf`)
isH2 x = TL.take 2 x == "==" && TL.drop 2 (TL.take 3 x) /= "="

procTitle :: T -> T
procTitle = TL.dropEnd (TL.length titlePost) . TL.drop (TL.length titlePre)

readStart :: T -> Ts -> [Ts]
readStart _ []     = []
readStart l (x:xs) = if isTitle x then readFromTitle l x xs else readStart l xs

readFromTitle :: T -> T -> Ts -> [Ts]
readFromTitle _ t []     = []
readFromTitle l t (x:xs) = if isTitle x
  then readFromTitle l x xs
  else if x == "==" <> l <> "=="
    then readFromSection l [] (procTitle t) xs
    else readFromTitle l t xs

readFromSection :: T -> Ts -> T -> Ts -> [Ts]
readFromSection _ _   _    []     = error "End of input in section"
readFromSection l acc acc1 (x:xs) = if isH2 x
  then (acc ++ [acc1]) : readStart l xs
  else if shaPre `TL.isPrefixOf` x
    then (acc ++ [TL.drop (TL.length textPost) acc1]) : readStart l xs
    else readFromSection l (acc ++ [acc1]) x xs

main :: IO ()
main = do
    [l] <- getArgs
    pages <- readStart (TL.pack l) . TL.lines <$> TL.getContents
    mapM_ (\(t:xs) -> putStrLn $ show t <> show (TL.unlines xs)) pages
