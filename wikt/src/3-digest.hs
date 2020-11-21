#include <h>

{-
sectionsToKeep, sectionsToKill :: [Text]
sectionsToKeep =
  [ "Noun"
  , "Verb"
  , "Usage notes"
  , "Synonyms"
  , "Derived terms"
  , "Related terms"
  , "Preposition"
  ]
-}
sectionsToKill :: [Text]
sectionsToKill =
  [ "Alternative forms"
  , "Conjugation"
  , "Descendants"
  , "Etymology"
  , "Futher reading"
  , "Pronunciation"
  , "Quotations"
  ]

isHeader l = T.take 1 l == "="

killPre p t = let n = T.length p in if T.take n t == p then T.drop n t else t

procEntry :: [Text] -> [Text]
procEntry [] = []
procEntry [l] = procEntry [l,  ""]
procEntry (l:l2:ls) = if isHeader l
  then let header = T.dropAround (== '=') l
           (section, rest) = span (\l -> T.take 1 l /= "=") ls
    in if header `elem` sectionsToKill
      then procEntry rest
      else (++ procEntry rest) $ case header of
        "Coordinate terms" ->
          ["CoT: " <> T.intercalate "" (map (killPre "* ") $ l2:section)]
        "Derived terms" -> if T.take 3 l2 == "pt|" then [] else  -- data bug
          ["DeT: " <> T.intercalate "" (map (killPre "* ") $ l2:section)]
        "Related terms" ->
          ["ReT: " <> T.intercalate "" (map (killPre "* ") $ l2:section)]
        "See Also" -> 
          ["See: " <> T.intercalate "" (map (killPre "* ") $ l2:section)]
        "Antonyms" -> 
          ["Ant: " <> T.intercalate "|" (map (killPre "* ") $ l2:section)]
        "Hyponyms" -> 
          ["Hypo: " <> T.intercalate "|" (map (killPre "* ") $ l2:section)]
        "Hypernyms" -> 
          ["Hyper: " <> T.intercalate "|" (map (killPre "* ") $ l2:section)]
        "Synonyms" -> 
          ["Syn: " <> T.intercalate "|" (map (killPre "* ") $ l2:section)]
        "Usage notes" -> "Usage: " <> l2 : section
        _ -> header <> ": " <> l2 : section
  else procEntry (l2:ls) --  error $ "procEntry: " ++ show l

procDT = map id . filter ((/= "{") . take 1)

f l = do
    let word, entry :: Text
        (word,l2):_ = reads $ T.unpack l
        entry = T.replace "|pt|" "" .
            T.replace "{{" "<" . T.replace "}}" ">" . 
            -- T.replace "{{em|intr=1" "<" .    -- actually this talks about em
            -- T.replace " {{em|intr=1}}" "" .  -- preposition
            T.replace "{{top2}}" "" .
            T.replace "{{mid2}}" "" .
            T.replace "{{bot2}}" "" .
            T.replace "{{top3}}" "" .
            T.replace "{{mid3}}" "" .
            T.replace "{{bot3}}" "" .
            T.replace "{{top4}}" "" .
            T.replace "{{mid4}}" "" .
            T.replace "{{bot4}}" "" .
            T.replace "{{top5}}" "" .
            T.replace "{{mid5}}" "" .
            T.replace "{{bot5}}" "" .
            T.replace "{{l|en|" "<" .
            T.replace "{{l|pt|" "<" .
            T.replace "{{lb|pt|" "<" .
            T.replace "{{ux|pt|" "<" .
            T.replace "{{uxi|pt|" "<" .
            T.replace "{{syn|pt|" "<" .
            T.replace "{{der2|pt|" "<" .
            T.replace "{{rel3" "<" .
            T.replace "{{rel3|pt" "<" .
            T.replace "{{sense|" "<" .
            T.replace "{{gloss|" "<" .
            T.replace "{{indtr|pt|" "<" .
            T.replace "|inline=1}}" ">" .
            T.replace "''" "" . -- I went with "" not "\"" here.
            T.replace "'''" "" . -- I went with "" not "\"" here.
            T.replace "translation=" "" .
            T.replace "{{senseid|pt|" "<" $
            read l2
    T.putStrLn word
    mapM_ T.putStrLn $ procEntry $ filter (/= "----") $ filter (/= "") $
        T.lines entry

main :: IO ()
main = do
    ls <-
        filter (not . ("\"Appendix:"  `T.isPrefixOf`)) . 
        filter (not . ("\"Wiktionary:"  `T.isPrefixOf`)) . 
        T.lines <$> T.getContents
    {-
    ls <- T.lines <$> T.readFile "out/pt"
    let rs = drop 1 $ sortBy (flip $ comparing T.length) $
            filter (not . ("\"Wiktionary:"  `T.isPrefixOf`)) ls
    mapM_ f $ take 100 rs
    -}
    mapM_ f ls
