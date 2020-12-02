#include <h>

rDir = "/home/danl/r"
dataDir = rDir </> "data"
aFile = dataDir </> "avail/r2"
sFile = dataDir </> "standalone-and-seasons-page/cur/rtp-tv.html"
needle1a = "          <a href=\""
needle1b = "https://www.rtp.pt/play/p"
needle1  = needle1a <> needle1b
needle2  = "              <h4 class=\"episode-title\"> "
needle2b = " episódios</span>"
needle2c = " | "
needle3  = "            <meta itemprop=\"name\" content=\""
needle4  = "            <meta itemprop=\"description\" content=\""
len1a = T.length needle1a
len2b = T.length needle2b
len2c = T.length needle2c
len3  = T.length needle3
len4  = T.length needle4

type PNum = Int
data AInfo = AInfo
  { aIsAvail :: !Bool
  , aHasSubs :: !Bool
  , aEpSecs  :: !Float
  , aUrl     :: !Text
  , aDesc    :: !Text
  } deriving (Eq, Ord)
data SInfo = SInfo
  { sName    :: !Text
  , sEpCount :: !Int
  , sSeason  :: !Text
  , sNextEp  :: !Text
  , sDesc    :: !Text
  -- , sUrl     :: !Text
  } deriving (Eq, Ord)

urlToNum :: Text -> PNum
urlToNum = read . T.unpack . T.takeWhile isDigit . T.dropWhile (not . isDigit)

procA :: Text -> (Int, AInfo)
procA l = (urlToNum u, AInfo (a == "1") (b == "1") epSecs u desc) where
    _:a:b:c:u:rest = T.words l
    epSecs = case c of
      "NaN" -> 0
      "?" -> 0
      _ -> read $ T.unpack c
    desc = read $ T.unpack $ T.unwords rest

reps = T.replace "&amp;amp;" "&" . T.replace "&quot;" "\""

procS :: [Text] -> [(Int, SInfo)]
procS = step1 where
  step1 [] = []
  step1 (l:ls) = if needle1 `T.isPrefixOf` l
    then step2 (T.takeWhile (/= '"') $ T.drop len1a l) ls
    else step1 ls
  step2 r1 [] = error $ "No episode title! " ++ show r1
  step2 r1 (l:ls) = if needle2 `T.isPrefixOf` l
    then let
      l2 = T.dropEnd len2b l
      l3 = T.dropWhileEnd isDigit l2
      wat1 :: String
      wat1 = T.unpack $ T.takeWhileEnd isDigit l2
      wat2 :: Int
      wat2 = readNote (error "Couldn't read digits to Int.. impossible!") wat1
      (epCount, season) = if needle2b `T.isSuffixOf` l
        then (wat2, if needle2c `T.isSuffixOf` l3
          then T.takeWhileEnd (/= ' ') $ T.dropEnd len2c l3 else "")
        else (1, "")
      in step3 r1 (epCount, season) ls
    else step2 r1 ls
  step3 _ _ [] = error "No next episode!"
  step3 r1 r2 (l:ls) = if needle3 `T.isPrefixOf` l
    then step4 r1 r2 (T.takeWhile (/= '"') $ T.drop len3 l) ls
    else step3 r1 r2 ls
  step4 _ _ _ [] = error "No series description!"
  step4 r1 r2 r3 (l:ls) = if needle4 `T.isPrefixOf` l
    then let 
      w = T.words r3
      wL = length w
      n = wL - if wL > 5 && w !! (wL - 5) == "ep." then 5 else 3
      (name, nextEp) = first T.unwords . second T.unwords $ splitAt n w
      desc = T.takeWhile (/= '"') $ T.drop len4 l
      (epCount, season) = r2
      s = SInfo {sName = reps name, sEpCount = epCount, sSeason = season,
          sNextEp = nextEp, sDesc = reps desc}
          -- sNextEp = nextEp, sDesc = reps desc, sUrl = r1}
      in (urlToNum r1, s) : step1 ls
    else step4 r1 r2 r3 ls

tSecs s = if s > 3600
  then T.pack (show . round $ s / 3600) <> "h"
  else T.pack (show . round $ s / 60) <> "m"

main :: IO ()
main = do
    a <- IM.fromList .
        filter (aHasSubs . snd) . 
        filter (aIsAvail . snd) . 
        map procA .
        T.lines <$> T.readFile aFile
    s <- IM.fromList . 
        filter ((== "2020") . last . T.words . sNextEp . snd) . 
        procS . T.lines <$> T.readFile sFile
    mapM_ (\(secs, as) -> T.putStrLn . T.unlines $
        tSecs secs <> " " <> sName (snd $ head as) : concatMap (\(a, s) ->
        [ "- S[" <> sSeason s <> "] " <>
          T.pack (show $ sEpCount s) <> "ep[" <>
          sNextEp s <> "] " <> (if aHasSubs a then "" else "No") <> 
          "Subs"
        , aUrl a
        , "Desc: " <> sDesc s
        , "Syno: " <> aDesc a]) as) .
        sortBy (flip compare) .
        map (\as -> 
            ( sum $ map (\(a, s) -> aEpSecs a * realToFrac (sEpCount s)) as
            , as)) .
        HM.elems .
        HM.fromListWith (++) . map (\(a, s) -> (sName s, [(a, s)])) .
        IM.elems $ IM.intersectionWith (,) a s
