-- Usage: bzless ~/data/wikt/en.xml.bz2 | rhs a.hs > out.txt
#include <h>

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

readStart :: T -> Ts -> [Ts]
readStart _ []     = []
readStart l (x:xs) = if isTitle x then readFromTitle l x xs else readStart l xs

readFromTitle :: T -> T -> Ts -> [Ts]
readFromTitle _ t []     = []
readFromTitle l t (x:xs) = if isTitle x
  then readFromTitle l x xs
  else if x == "==" <> l <> "=="
    then readFromSection l [t] xs -- TODO t extract title
    else readFromTitle l t xs

readFromSection :: T -> Ts -> Ts -> [Ts]
readFromSection _ _   []     = error "End of input in section"
readFromSection l acc (x:xs) = if isH2 x
  then acc : readStart l xs
  else if shaPre `TL.isPrefixOf` x
    then acc : readStart l xs  -- TODO extract acc last line
    else readFromSection l (acc ++ [x]) xs

main :: IO ()
main = do
    [l] <- getArgs
    pages <- readStart (TL.pack l) . TL.lines <$> TL.getContents
    mapM_ (\(t:xs) -> putStrLn $ show t <> show (TL.unlines xs)) pages
