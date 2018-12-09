#include <h>

data Ci = Ci {
    ciNum :: Int,
    ciHanzi :: DT.Text,
    ciDef :: DT.Text} deriving (Show, Generic)

textToJson t =
    "\"" <> DT.replace "\"" "\\\"" (DT.replace "\\" "\\\\" t) <> "\""

ciToJson (Ci n h d) = "[" <>
    DT.pack (show n) <> "," <> textToJson h <> "," <> textToJson d <> "]"

cisToJson cis = "[" <> DT.intercalate "," (map ciToJson cis) <> "]"

homonymSetToJson (h, cis) = "[" <> textToJson h <> "," <> cisToJson cis <> "]"

homonymSetsToJson hs = "[" <>
    DT.intercalate "," (map homonymSetToJson hs)  <> "]"

cleanDef =
    DT.replace "<\21160>" "V." .
    DT.replace "<\21517>" "N." .
    DT.replace "<\37327>" "M." .
    DT.replace "<\24418>" "S.V." .
    DT.replace "<\21475>" "COLL." .
    DT.replace "<\21103>" "ADV." .
    DT.replace "<\36830>" "CONJ." .
    DT.replace "&lt;" "<" .
    DT.replace "&gt;" ">" .
    DT.replace "&nbsp;" " "

-- FIXME: fix this
set33To23 :: DT.Text -> DT.Text
set33To23 s = if DT.null threeAndRest || DT.null sndThreeAndRest then s else
    beforeThree <> "2" <> beforeSndThree <> set33To23 sndThreeAndRest 
  where
    (beforeThree, threeAndRest) = DT.break (== '3') s
    (beforeSndThree, sndThreeAndRest) =
        DT.break (== '3') $ DT.tail threeAndRest

procLine :: Int -> DT.Text -> [(DT.Text, [Ci])]
procLine n l = [(p, [Ci n hanzi d]) | (p, d) <- zip pinyins defs]
  where
    hanzi:pinyin:def:_ = DT.splitOn "\t" l
    pinyins = map set33To23 $ DT.splitOn " \\ " pinyin
    defs = map cleanDef $ DT.splitOn " \\ " def

--procEl :: [(Int, DT.Text, DT.Text)] -> Maybe (Int, [(DT.Text, DT.Text)])
--procEl l = if any over2300 l then Just $ map bcOfAbc $ reverse l else Nothing
procEl l = Just (minimum $ map aOfAbc l, reverse l)
  where
    over2300 (n, _, _) = n > 2300
    aOfAbc (a, _, _) = a
    bcOfAbc (_, b, c) = (b, c)

main = do
    m <- sortBy (compare `on` ciNum . head . snd) .
        HMS.toList . HMS.map reverse . HMS.fromListWith (++) . concat .
        zipWith procLine [1..] . DT.lines . DTE.decodeUtf8 . BSL.toStrict <$>
        HSH.run ("bzcat" :: String, ["all.txt.bz2" :: String])
    DTI.writeFile "entries.js" $ "var entries = " <>
        homonymSetsToJson (filter (any (> 2300) . map ciNum . snd) m) <> ";"
