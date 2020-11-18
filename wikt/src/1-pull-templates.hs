-- Usage: bzless ~/data/wikt/en.xml.bz2 | rhs a.hs > out.txt
#include <h>

type Ts = [T]
type T = TL.Text

titlePre, textPre, shaPre, titlePost, textPost :: T
titlePre  = "    <title>Template:"
textPre   = "      <text bytes=\""
shaPre    = "      <sha1>"
titlePost = "</title>"
textPost  = "</text>"

isTitle :: T -> Bool
isTitle = (titlePre `TL.isPrefixOf`)

readStart :: Ts -> [Ts]
readStart [] = []
readStart (x:xs) = if isTitle x then readFromTitle x xs else readStart xs

readFromTitle :: T -> Ts -> [Ts]
readFromTitle t [] = error "End of input after title before text"
readFromTitle t (x:xs) = if textPre `TL.isPrefixOf` x
  then readFromText [t] xs -- TODO extract t
  else readFromTitle t xs

readFromText :: Ts -> Ts -> [Ts]
readFromText acc [] = error "End of input in section"
readFromText acc (x:xs) = if shaPre `TL.isPrefixOf` x
  then acc : readStart xs  -- TODO extract acc last line
  else readFromText (acc ++ [x]) xs

main :: IO ()
main = do
    pages <- readStart . TL.lines <$> TL.getContents
    mapM_ (\(t:xs) -> putStrLn $ show t <> show (TL.unlines xs)) pages
