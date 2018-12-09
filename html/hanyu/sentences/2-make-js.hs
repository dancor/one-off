#include <h>

import Codec.Serialise

import WdPinyin

readCedictGloss :: DT.Text -> (DT.Text, DT.Text)
readCedictGloss t =
    if null slashParts || null toMinOver
    then error $ "readCedictGloss: " ++ DT.unpack t
    else (simplifiedChinese, gloss)
  where
    _:simplifiedChinese:_ = DT.words t
    slashParts = DT.splitOn "/" t
    _:defs = slashParts
    toMinOver = 
        map (\x -> if "to " `DT.isPrefixOf` x then DT.drop 3 x else x) .
        map (last . DT.splitOn ") ") $
        map (head . DT.splitOn " (") $
        filter (not . (== "\r")) $
        filter (not . ("CL:" `DT.isPrefixOf`)) defs
    gloss = DT.replace " " "-" $ minimumBy (compare `on` DT.length) toMinOver

wdPinyinGlossToAeson (WdPinyinGloss wd pinyin gloss) =
    Ae.Array $ Vec.fromList $ map Ae.String [wd, pinyin, gloss]

wdPinyinGlossesToAeson = Ae.Array . Vec.fromList . map wdPinyinGlossToAeson

wdPinyinAddGloss glossMap (WdPinyin wd py) = WdPinyinGloss wd py
    (fromMaybe wd $ HMS.lookup wd glossMap)

main = do
    {-
    hsk4Set <- HSet.fromList . take 1200 . map (DT.takeWhile (/= '\t')) .
        DT.lines <$> DTI.readFile "/home/danl/all.csv.txt"
        -}
    glossPairs <- map readCedictGloss . 
        filter (not . ("#" `DT.isPrefixOf`)) . DT.lines <$> DTI.readFile
        "/home/danl/p/l/melang/lang/zh/cedict/cedict_1_0_ts_utf-8_mdbg.txt"
    let glossMap = HMS.fromList $ glossPairs ++
            [ ("了", "le")
            , ("的", "de")
            , ("个", "ge")
            , ("是", "is")
            , ("好", "good")
            , ("。", ".")
            , ("，", ",")
            , ("！", "!")
            ]
    (n1s, wdPinyinSents, eSents) <- deserialise <$> BSL.readFile
        "/home/danl/p/one-off/www/hanyu/tatoeba/sentence.info"
    print $ length wdPinyinSents
    {-
    mapM_ (DTI.putStrLn . DT.intercalate " ") $
        map (map (\(WdPinyin wd _) -> wd)) $ filter
        (all (\(WdPinyin wd _) -> wd `HSet.member` hsk4Set)) wdPinyinSents
        -}
    let wdPinyinGlossSents =
            map (map (wdPinyinAddGloss glossMap)) wdPinyinSents
        prepEntry count n1 wdPinyinGlosses eSent =
            [ Ae.String n1
            , wdPinyinGlossesToAeson wdPinyinGlosses
            , Ae.String eSent
            ]
        entries = zipWith4 prepEntry [1..] n1s wdPinyinGlossSents eSents
    BSLC.writeFile "/home/danl/p/one-off/www/hanyu/tatoeba/entries.js" $
        "var entries = " <> Ae.encode entries <> ";"
