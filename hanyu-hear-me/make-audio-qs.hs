#include <h>

data Ci = Ci {
    ciNum :: Int,
    ciHanzi :: DT.Text,
    ciDef :: DT.Text} deriving (Show, Generic)

instance Ae.ToJSON Ci
instance Ae.FromJSON Ci

cleanDef =
    DT.replace "<\21160>" "V." .
    DT.replace "<\21517>" "N." .
    DT.replace "<\37327>" "M." .
    DT.replace "<\24418>" "S.V." .
    DT.replace "<\21475>" "COLL." .
    DT.replace "<\21103>" "ADV." .
    DT.replace "<\36830>" "CONJ."

procLine :: Int -> DT.Text -> (DT.Text, [Ci])
procLine n l = (pinyin, [Ci n hanzi (cleanDef def)])
  where hanzi:pinyin:def:_ = DT.splitOn "\t" l

--procEl :: [(Int, DT.Text, DT.Text)] -> Maybe (Int, [(DT.Text, DT.Text)])
--procEl l = if any over2300 l then Just $ map bcOfAbc $ reverse l else Nothing
procEl l = Just (minimum $ map aOfAbc l, reverse l)
  where
    over2300 (n, _, _) = n > 2300
    aOfAbc (a, _, _) = a
    bcOfAbc (_, b, c) = (b, c)

main = do
    m <- sortBy (compare `on` ciNum . head . snd) .
        HMS.toList . HMS.map reverse . HMS.fromListWith (++) .
        zipWith procLine [1..] . DT.lines . DTE.decodeUtf8 . BSL.toStrict <$>
        HSH.run ("bzcat" :: String, ["all.txt.bz2" :: String])
    BSL.writeFile "homonymSets.js" . ("var homonymSets = " <>) . (<> ";\n") .
        Ae.encode $ filter (any (> 2300) . map ciNum . snd) m
