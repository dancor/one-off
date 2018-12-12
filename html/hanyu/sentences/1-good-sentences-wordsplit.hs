#include <h>

import Codec.Serialise
import Lang.Zh.Cedict

import WdPinyinGloss

procLines :: FilePath -> [String] -> IO [Text]
procLines cmd args = do
    (_, Just hOut, _, _) <-
        createProcess (proc cmd args) {std_out = CreatePipe}
    map TL.toStrict . TL.lines <$> TL.hGetContents hOut

readDataResourceLines :: String -> IO [Text]
readDataResourceLines r = do
    home <- getHomeDirectory
    readResourceLines $ home </> "data" </> r

readResourceLines :: FilePath -> IO [Text]
readResourceLines r = do
    e <- doesFileExist r
    let tryReaders [] = error $ "Could not read resource: " ++ r
        tryReaders ((ext,cmd):readers) = do
            let rExt = r ++ ext
            e <- doesFileExist $ rExt
            if e then procLines cmd [rExt] else tryReaders readers
    if e then map TL.toStrict . TL.lines <$> TL.readFile r
      else tryReaders [(".xz", "xzcat"), (".gz", "zcat")]

procLine :: [Text] -> (Int, Text)
procLine [num, _, sent] = (read $ T.unpack num, sent)
procLine x = error $ "procLine: " ++ show x

splitCols :: Text -> [Text]
splitCols = T.splitOn "\t"

readNumToSentMap :: String -> IO (HashMap Int Text)
readNumToSentMap r = do
    HM.fromList . map (procLine . splitCols) <$> readDataResourceLines r

loadTatoeba :: IO [(Int, Text, Text)]
loadTatoeba = do
    l1 <- readNumToSentMap ("tatoeba" </> "simp-cmn.csv")
    l2 <- readNumToSentMap ("tatoeba" </> "eng.csv")
    nums <- HM.toList . HM.fromListWith Set.union .
        map (\(a, b) -> (a, Set.singleton b)) .
        filter (\(a, b) -> a `HM.member` l1 && b `HM.member` l2) .
        map (\[a, b] -> (read $ T.unpack a, read $ T.unpack b)) .
        map splitCols <$> readDataResourceLines ("tatoeba" </> "links.csv")
    let n1s = map fst nums
        zhSentences =
            [fromJust (HM.lookup n1 l1) | n1 <- n1s]
        enSentences =
            [ maximumBy (compare `on` T.length)
              [fromJust (HM.lookup n2 l2) | n2 <- Set.toList n2s]
            | (_, n2s) <- nums
            ]
    return $ zip3 n1s zhSentences enSentences

--

wdGloss :: Cedict -> Text -> [WdPinyinGloss]
wdGloss dict wd = case HM.lookup wd dict of
  Just (CedictEntry p g) -> [WdPinyinGloss wd p g]
  Nothing -> if T.length wd == 1
    then [WdPinyinGloss wd "" ""]
    else concatMap (wdGloss dict . T.singleton) $ T.unpack wd

procTreeLine :: Cedict -> Text -> [WdPinyinGloss]
procTreeLine dict l = wdGloss dict wd where [_, _, _, wd] = T.splitOn "\t" l

getPinyins :: Cedict -> [Text] -> IO [[WdPinyinGloss]]
getPinyins dict sentences = do
    home <- getHomeDirectory
    (Just hIn, Just hOut, _, _) <- createProcess
        (proc (home </> "p/l/melang/zh-sentence-trees.py") [])
        {std_in = CreatePipe, std_out = CreatePipe}
    Conc.forkIO $ mapM_ (T.hPutStrLn hIn) sentences >> hClose hIn
    ls <- TL.lines <$> TL.hGetContents hOut
    return $ map (concatMap (procTreeLine dict . TL.toStrict)) $
            Spl.splitWhen TL.null ls

checkDropStart :: TL.Text -> TL.Text -> TL.Text
checkDropStart needle x =
    let (shouldBeNeedle, ret) = TL.splitAt (TL.length needle) x
    in if shouldBeNeedle == needle
      then ret
      else error $ "checkDropStart: Wanted " ++ show needle ++ " but got " ++
        show shouldBeNeedle

checkDropEnd :: TL.Text -> TL.Text -> TL.Text
checkDropEnd needle x =
    let (ret, shouldBeNeedle) = TL.splitAt (TL.length x - TL.length needle) x
    in if shouldBeNeedle == needle
      then ret
      else error $ "checkDropStart: Wanted " ++ show needle ++ " but got " ++
        show shouldBeNeedle

readTmxGz :: String -> IO [(Int, Text, Text)]
readTmxGz filename = do
    let enStart = "      <tuv xml:lang=\"en\"><seg>"
        zhStart = "      <tuv xml:lang=\"zh\"><seg>"
        end = "</seg></tuv>"
        procChunk :: [TL.Text] -> (Int, Text, Text)
        procChunk (enL:zhL:_) =
            (hash zhSentence, TL.toStrict zhSentence, TL.toStrict enSentence)
          where
            enSentence = checkDropEnd end $ TL.drop (TL.length enStart) enL
            zhSentence = checkDropEnd end $ checkDropStart zhStart zhL
    (_, Just hOut, _, _) <-
        createProcess (proc "zcat" [filename]) {std_out = CreatePipe}
    ls <- TL.lines <$> TL.hGetContents hOut
    let chunks :: [[TL.Text]]
        chunks = tail $
            Spl.split (Spl.keepDelimsL $ Spl.whenElt (enStart `TL.isPrefixOf`))
            ls
    return $ map procChunk chunks

loadMultiUN = do
    enZhDir <- (\x -> x </> "data" </> "en-zh") <$> getHomeDirectory
    readTmxGz (enZhDir </> "MultiUN.tmx.gz")
    --readTmxGz (enZhDir </> "MultiUNSample.tmx.gz")

loadOpenSubtitles = do
    enZhDir <- (\x -> x </> "data" </> "en-zh") <$> getHomeDirectory
    readTmxGz (enZhDir </> "OpenSubtitles.tmx.gz")

spaceCol :: [Text] -> [Text]
spaceCol [] = []
spaceCol rows = zipWith
    (\row width -> row <> T.replicate (maxWidth - width) " ") rows widths
  where
    widths = map (sum . map wcwidth . T.unpack) rows
    maxWidth = maximum widths

lol en wdPinyinGlosses = do
    --when (any (\(WdPinyinGloss w _ _) -> w == "哪怕") wdPinyinGlosses) $ do
        T.putStrLn en
        mapM_ (T.putStrLn . T.intercalate " ") $ transpose
            [spaceCol [w, p, g] | WdPinyinGloss w p g <- wdPinyinGlosses]
        T.putStrLn ""
        hFlush stdout

main :: IO ()
main = do
    dict <- loadCedictGlossMap
    (nums, zhSentences, enSentences) <- unzip3 <$> loadTatoeba
    --(nums, zhSentences, enSentences) <- unzip3 <$> loadOpenSubtitles
    --(nums, zhSentences, enSentences) <- unzip3 <$> loadMultiUN
    {-
    (nums, zhSentences, enSentences) <- unzip3 . concat <$>
        sequence [loadTatoeba, loadOpenSubtitles, loadMultiUN]
    -}
    wdPinyinGlossSentences <- getPinyins dict zhSentences
    home <- getHomeDirectory
    zipWithM_ lol enSentences wdPinyinGlossSentences

    {-
    hsk4Set <- HSet.fromList . take 1200 . map (DT.takeWhile (/= '\t')) .
        DT.lines <$> DTI.readFile "/home/danl/all.csv.txt"
    mapM_ (DTI.putStrLn . DT.intercalate " ") $
        map (map (\(WdPinyin wd _) -> wd)) $ filter
        (all (\(WdPinyin wd _) -> wd `HSet.member` hsk4Set)) wdPinyinSents
    -}
    {-
    let prepEntry count num wdPinyinGlosses eSentence =
            [ Ae.String $ T.pack $ show (num :: Int)
            , wdPinyinGlossesToAeson wdPinyinGlosses
            , Ae.String eSentence
            ]
        entries = zipWith4 prepEntry
            [1..] nums wdPinyinGlossSentences eSentences
    BL.writeFile "/home/danl/p/one-off/html/hanyu/sentences/entries.js" $
        "var entries = " <> Ae.encode entries <> ";"
    -}
