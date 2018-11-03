#include <h>

sEx :: String
sEx = "我的行李丢了。"

js s = BSL.concat [
    "var pinyinify = require('pinyinify');",
    "var sentences = " <> Ae.encode s <> ";",
    "for (var i = 0; i < sentences.length; i++) {",
    "  var sentence = sentences[i];",
    "  wds = pinyinify(sentence, true).pinyinSegmentsSyllables;",
    "  for (var wdI = 0; wdI < wds.length; wdI++) {",
    "    var wd = wds[wdI];",
    "    console.log(wd.join(' '));",
    "  }",
    "  console.log('ZIFYRA');",
    "}"]

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

getPinyins :: [String] -> IO [String]
getPinyins s = do
    setCurrentDirectory "/home/danl/p/one-off/www/hanyu/node_modules/pinyinify"
    (_, out, _err) <- readProcessWithExitCode "nodejs" []
        (BSLU.toString $ js s)
    return $
        map (intercalate " " . map (intercalate "" . map pyToNum . words)) $
        Spl.splitWhen (== "ZIFYRA") $
        lines out

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
    print $ length nums
    let mandarinSentences = [fromJust (HMS.lookup n1 l1) | (n1, _) <- nums]
    pinyinSentences <- getPinyins $ map DT.unpack $ mandarinSentences
    let prepEntry count mandarinSentence pinyinSentence (n1, n2s) = do
            when (count `mod` 100 == 0) $ putStrLn $ show count ++ " / 40k"
            return 
                [ n1
                , mandarinSentence
                , DT.pack pinyinSentence
                , maximumBy (compare `on` DT.length)
                  [fromJust (HMS.lookup n2 l2) | n2 <- n2s]
                ]
    entries <- sequence $
       zipWith4 prepEntry [1..] mandarinSentences pinyinSentences nums
    print $ last $ show entries
    BSLC.writeFile "/home/danl/p/one-off/www/hanyu/tatoeba/entries.js" $
        "var entries = " <> Ae.encode entries <> ";"
