{-# LANGUAGE OverloadedStrings #-}

module Load
  ( loadMultiUN
  , loadOpenSubtitles
  , loadTatoeba
  ) where

import Data.Function
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.List.Split as Spl
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Directory
import System.FilePath
import System.Process


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

splitCols :: Text -> [Text]
splitCols = T.splitOn "\t"

procLine :: [Text] -> (Int, Text)
procLine [num, _, sent] = (read $ T.unpack num, sent)
procLine x = error $ "procLine: " ++ show x

procLines :: FilePath -> [String] -> IO [Text]
procLines cmd args = do
    (_, Just hOut, _, _) <-
        createProcess (proc cmd args) {std_out = CreatePipe}
    map TL.toStrict . TL.lines <$> TL.hGetContents hOut

readNumToSentMap :: String -> IO (HashMap Int Text)
readNumToSentMap r = do
    HM.fromList . map (procLine . splitCols) <$> readDataResourceLines r

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
            e2 <- doesFileExist $ rExt
            if e2 then procLines cmd [rExt] else tryReaders readers
    if e then map TL.toStrict . TL.lines <$> TL.readFile r
      else tryReaders [(".xz", "xzcat"), (".gz", "zcat")]

readTmxGz :: String -> IO [(Int, Text, Text)]
readTmxGz filename = do
    let enStart = "      <tuv xml:lang=\"en\"><seg>"
        zhStart = "      <tuv xml:lang=\"zh\"><seg>"
        end = "</seg></tuv>"
        procChunk :: Int -> [TL.Text] -> (Int, Text, Text)
        procChunk i (enL:zhL:_) =
            (i, TL.toStrict zhSentence, TL.toStrict enSentence)
          where
            enSentence = checkDropEnd end $ TL.drop (TL.length enStart) enL
            zhSentence = checkDropEnd end $ checkDropStart zhStart zhL
        procChunk i x = error $ "procChunk: " ++ show i ++ " " ++ show x
    (_, Just hOut, _, _) <-
        createProcess (proc "zcat" [filename]) {std_out = CreatePipe}
    ls <- TL.lines <$> TL.hGetContents hOut
    let chunks :: [[TL.Text]]
        chunks = tail $
            Spl.split (Spl.keepDelimsL $ Spl.whenElt (enStart `TL.isPrefixOf`))
            ls
    return $ zipWith procChunk [1..] chunks

loadMultiUN :: IO [(Int, Text, Text)]
loadMultiUN = do
    enZhDir <- (\x -> x </> "data" </> "en-zh") <$> getHomeDirectory
    readTmxGz (enZhDir </> "MultiUN.tmx.gz")
    --readTmxGz (enZhDir </> "MultiUNSample.tmx.gz")

loadOpenSubtitles :: IO [(Int, Text, Text)]
loadOpenSubtitles = do
    enZhDir <- (\x -> x </> "data" </> "en-zh") <$> getHomeDirectory
    readTmxGz (enZhDir </> "OpenSubtitles.tmx.gz")

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
