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

pyPullNum acc [] n = (acc, n)
pyPullNum acc ('ā':xs) _ = pyPullNum (acc ++ ['a']) xs 1
pyPullNum acc ('á':xs) _ = pyPullNum (acc ++ ['a']) xs 2
pyPullNum acc ('ǎ':xs) _ = pyPullNum (acc ++ ['a']) xs 3
pyPullNum acc ('à':xs) _ = pyPullNum (acc ++ ['a']) xs 4
pyPullNum acc ('ē':xs) _ = pyPullNum (acc ++ ['e']) xs 1
pyPullNum acc ('é':xs) _ = pyPullNum (acc ++ ['e']) xs 2
pyPullNum acc ('ě':xs) _ = pyPullNum (acc ++ ['e']) xs 3
pyPullNum acc ('è':xs) _ = pyPullNum (acc ++ ['e']) xs 4
pyPullNum acc ('ī':xs) _ = pyPullNum (acc ++ ['i']) xs 1
pyPullNum acc ('í':xs) _ = pyPullNum (acc ++ ['i']) xs 2
pyPullNum acc ('ǐ':xs) _ = pyPullNum (acc ++ ['i']) xs 3
pyPullNum acc ('ì':xs) _ = pyPullNum (acc ++ ['i']) xs 4
pyPullNum acc ('ō':xs) _ = pyPullNum (acc ++ ['o']) xs 1
pyPullNum acc ('ó':xs) _ = pyPullNum (acc ++ ['o']) xs 2
pyPullNum acc ('ǒ':xs) _ = pyPullNum (acc ++ ['o']) xs 3
pyPullNum acc ('ò':xs) _ = pyPullNum (acc ++ ['o']) xs 4
pyPullNum acc ('ū':xs) _ = pyPullNum (acc ++ ['u']) xs 1
pyPullNum acc ('ú':xs) _ = pyPullNum (acc ++ ['u']) xs 2
pyPullNum acc ('ǔ':xs) _ = pyPullNum (acc ++ ['u']) xs 3
pyPullNum acc ('ù':xs) _ = pyPullNum (acc ++ ['u']) xs 4
pyPullNum acc ('ǖ':xs) _ = pyPullNum (acc ++ ['v']) xs 1
pyPullNum acc ('ǘ':xs) _ = pyPullNum (acc ++ ['v']) xs 2
pyPullNum acc ('ǚ':xs) _ = pyPullNum (acc ++ ['v']) xs 3
pyPullNum acc ('ǜ':xs) _ = pyPullNum (acc ++ ['v']) xs 4
pyPullNum acc ('ü':xs) n = pyPullNum (acc ++ ['v']) xs n
pyPullNum acc (x:xs) n = pyPullNum (acc ++ [x]) xs n

pyToNum syllable = if any isAlpha syllable
  then let (syllable', n) = pyPullNum "" syllable 5 in syllable' ++ show n
  else syllable

wdPinyinGlossToAeson (WdPinyinGloss wd pinyin gloss) =
    Ae.Array $ Vec.fromList $ map Ae.String [wd, pinyin, gloss]

wdPinyinGlossesToAeson = Ae.Array . Vec.fromList . map wdPinyinGlossToAeson

wdPinyinAddGloss glossMap (WdPinyin wd py) = WdPinyinGloss wd py
    (fromMaybe wd $ HMS.lookup wd glossMap)

procSentJsLine l = WdPinyin (DT.pack wd)
    (DT.pack $ intercalate "" $ map pyToNum pinyins)
  where
    wd:pinyins = words l

js s = BSL.concat [
    "var pinyinify = require('pinyinify');",
    "var sentences = " <> Ae.encode s <> ";",
    "for (var i = 0; i < sentences.length; i++) {",
    "  var sentence = sentences[i];",
    "  var res = pinyinify(sentence, true);",
    "  words = res.segments;",
    "  pinyins = res.pinyinSegmentsSyllables;",
    "  for (var j = 0; j < words.length; j++) {",
    "    console.log(words[j] + ' ' + pinyins[j].join(' '));",
    "  }",
    "  console.log('ZIFYRA');",
    "}"]

getPinyins :: [DT.Text] -> IO [[WdPinyin]]
getPinyins s = do
    setCurrentDirectory "/home/danl/p/one-off/www/hanyu/node_modules/pinyinify"
    (_, out, _err) <- readProcessWithExitCode "nodejs" []
        (BSLU.toString $ js $ map DT.unpack s)
    return $ map (map procSentJsLine) $ Spl.splitWhen (== "ZIFYRA") $ lines out

procLine [num, _, sent] = (num, sent)
procLine x = error $ "procLine: " ++ show x

splitCols = DT.splitOn "\t"

readNumToSentMap filename = 
    HMS.fromList . map (procLine . splitCols) .
    DT.lines . DTE.decodeUtf8 <$>
    HSH.run ("xzcat" :: String , [filename :: String])

main = do
    tatDir <- (\x -> x </> "data" </> "tatoeba") <$> getHomeDirectory
    l1 <- readNumToSentMap $ tatDir </> "simp-cmn.csv.xz"
    l2 <- readNumToSentMap $ tatDir </> "eng.csv.xz"
    nums <- HMS.toList . HMS.fromListWith (++) . map (\[a, b] -> (a, [b])) .
        filter (\[a, b] -> a `HMS.member` l1 && b `HMS.member` l2) .
        map splitCols . DT.lines . DTE.decodeUtf8 <$>
        HSH.run ("xzcat" :: String, [tatDir </> "links.csv.xz" :: String]) 
    let n1s = map fst nums
        mandarinSentences = [fromJust (HMS.lookup n1 l1) | n1 <- n1s]
        englishSentences = 
            [ maximumBy (compare `on` DT.length)
              [fromJust (HMS.lookup n2 l2) | n2 <- n2s]
            | (_, n2s) <- nums
            ]
    wdPinyinSents <- getPinyins mandarinSentences
    BSLC.writeFile "/home/danl/p/one-off/www/hanyu/tatoeba/sentence.info" $
        serialise $ (n1s, wdPinyinSents, englishSentences)
