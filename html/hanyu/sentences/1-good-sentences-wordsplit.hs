#include <h>

import Codec.Serialise

import Lang.Zh.WdPinyin

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

{-
procLine [num, _, sent] = (num, sent)
procLine x = error $ "procLine: " ++ show x

splitCols = DT.splitOn "\t"

readNumToSentMap filename = 
    HMS.fromList . map (procLine . splitCols) .
    DT.lines . DTE.decodeUtf8 <$>
    HSH.run ("xzcat" :: String , [filename :: String])

loadTatoebaSentences = do
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
    return (n1s, wdPinyinSents, englishSentences)
-}

checkDropStart needle x =
    if shouldBeNeedle == needle
      then ret
      else error $ "checkDropStart: Wanted " ++ show needle ++ " but got " ++
        shouldBeNeedle
  where
    (shouldBeNeedle, ret) = splitAt (DT.length needle) x

checkDropEnd needle x =
    if shouldBeNeedle == needle
      then ret
      else error $ "checkDropStart: Wanted " ++ show needle ++ " but got " ++
        shouldBeNeedle
  where
    (ret, shouldBeNeedl) = splitAt (DT.length x - DT.length needle) x

readTmxGz filename = do
    let enStart = "      <tuv xml:lang=\"en\"><seg>"
        zhStart = "      <tuv xml:lang=\"zh\"><seg>"
        end = "</seg></tuv>"
        procChunk (enL:zhL:_) = (hash zhSentence, zhSentence, enSentence)
          where
            enSentence = checkDropEnd end $ DT.drop (DT.length enStart) enL
            zhSentence = checkDropEnd end $ checkDropStart zhStart zhL
    ls <- DT.lines . DTE.decodeUtf8 <$>
        HSH.run ("zcat" :: String , [filename :: String])
    let chunks = tail $
            Spl.split (Spl.keepDelimsL $ Spl.whenElt (enStart `DT.isPrefixOf`))
            ls
    return $ map procChunk chunks

loadMultiUN = do
    enZhDir <- (\x -> x </> "data" </> "en-zh") <$> getHomeDirectory
    unzip3 <$> readTmxGz (enZhDir </> "MultiUN.tmx.gz")

loadOpenSubtitles = do
    enZhDir <- (\x -> x </> "data" </> "en-zh") <$> getHomeDirectory
    unzip3 <$> readTmxGz (enZhDir </> "OpenSubtitles.tmx.gz")

main = do
    (nums, zhSentences, enSentences)  <- loadMultiUN
    wdPinyinSents <- getPinyins mandarinSentences
    BSLC.writeFile "/home/danl/p/one-off/www/hanyu/tatoeba/sentence.info" $
        serialise (nums, wdPinyinSentences, enSentences)
