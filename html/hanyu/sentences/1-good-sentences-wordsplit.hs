{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent as Conc
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.List.Split as Spl
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time
import System.Directory
import System.FilePath
import System.IO
import System.Process

import Load
import Codec.Serialise

procTreeLine :: Text -> Text
procTreeLine l = wd where [_, _, _, wd] = T.splitOn "\t" l

getPinyins :: [Text] -> IO [[Text]]
getPinyins sentences = do
    home <- getHomeDirectory
    (Just hIn, Just hOut, _, _) <- createProcess
        (proc (home </> "p/l/melang/zh-sentence-trees.py") [])
        {std_in = CreatePipe, std_out = CreatePipe}
    _ <- Conc.forkIO $ mapM_ (T.hPutStrLn hIn) sentences >> hClose hIn
    ls <- TL.lines <$> TL.hGetContents hOut
    return . map (map (procTreeLine . TL.toStrict)) $ Spl.splitWhen TL.null ls

doSection :: Char -> FilePath -> Int -> [(Int, Text, Text)] -> IO ()
doSection _ _ _ [] = return ()
doSection letter outFilename i entries = do
    e <- doesFileExist (outFilename </> show i)
    if e
      then hPutStrLn stderr $ "Section " ++ show i ++ "/~400 already done."
      else do
        hPutStrLn stderr $ "Doing 100k section number " ++ show i ++ "/~400."
        let (nums, zhSentences, enSentences) = unzip3 curEntries
        t1 <- getCurrentTime
        wdPinyinGlossSentences <- getPinyins zhSentences
        BL.writeFile (outFilename </> show i) $ serialise $
            zip4 (repeat letter) nums wdPinyinGlossSentences enSentences
        t2 <- getCurrentTime
        hPutStrLn stderr $ "Section took " ++ show (t2 `diffUTCTime` t1) ++ "."
    doSection letter outFilename (i + 1) restEntries
  where
    (curEntries, restEntries) = splitAt 10000 entries

doSet :: Char -> (IO [(Int, Text, Text)]) -> FilePath -> IO ()
doSet letter loadFn outFilename = do
    entries <- loadFn
    doSection letter outFilename 1 entries

main :: IO ()
main = do
    doSet 'U' loadMultiUN "MultiUN"
