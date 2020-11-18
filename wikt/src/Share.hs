#include <hl>
module Share where
#include <hi>

type Ts = [T]
type T = TL.Text

titlePre, templatePre, textPre, shaPre, titlePost, textPost :: T
titlePre    = "    <title>"
templatePre = "    <title>Template:"
textPre   = "      <text bytes=\""
shaPre    = "      <sha1>"
titlePost = "</title>"
textPost  = "</text>"

isTitle, isH2 :: T -> Bool
isTitle = (titlePre `TL.isPrefixOf`)
isH2 x = TL.take 2 x == "==" && TL.drop 2 (TL.take 3 x) /= "="

procTitle :: T -> Maybe T
procTitle t = if titlePre `TL.isPrefixOf` t
  then Just $ TL.dropEnd (TL.length titlePost) $ TL.drop (TL.length titlePre) t
  else Nothing

procTemplate :: T -> Maybe T
procTemplate t = if templatePre `TL.isPrefixOf` t
  then Just $ TL.dropEnd (TL.length titlePost) $
    TL.drop (TL.length templatePre) t
  else Nothing

procText :: T -> Maybe T
procText t = if textPre `TL.isPrefixOf` t
  then case TL.uncons . TL.dropWhile (/= '>') $ TL.drop (TL.length textPre) t
       of   Just (_, t2) -> Just t2
            _ -> error $ "procText:" ++ show t
  else Nothing
