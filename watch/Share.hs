{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Share where

#include <h>

data Title = Title {unTitle :: String}
  deriving (Eq, Ord, Show)

spToU = map f where
    f ' ' = '_'
    f x = x

flattenTitle = Title .
    filter (`elem` '_':['a'..'z']++['0'..'9']) .
    takeWhile (/= '(') .
    replace " " "_" .
    replace "&" "and" .
    map toLower

type DescMap = HashMap String ([String], Desc)

data Desc = Desc
  { dTitle :: Text
  , dYear :: Int
  , dRating :: Text
  , dActors :: Text
  , dDescription :: Text
  , dGenres :: Text
  , dDuration :: Either Int Text
  , dRt :: Maybe (Int, Int)
  , dExtraGenres :: Text
  } deriving (Generic, Show)

instance Store.Store Desc

unenc :: Text -> Text
unenc =
    T.replace "&amp;" "&" .
    T.replace "&quot;" "\"" .
    T.replace "&#x27;" "'"

myInit [] = []
myInit x = init x

myHeadStr [] = ""
myHeadStr (x:xs) = x

myGrep args = do
    (_,out,_) <- readProcessWithExitCode "grep" ("-oP":args) ""
    return out

intToD :: Int -> Double
intToD = fromIntegral

pullDig :: String -> String
pullDig = filter isDigit

rtFileInfo f = do
    let scoreNeedle = "cag\\[score]\"(] = \"|:)\\K\\d*"
        yearNeedle = "cag\\[release]\"(]=\"|:)\\K(null|\\d*)"
    year <- readNote
        ("Error reading year digits after " ++ yearNeedle ++
        " in file: " ++ f) <$> myGrep [yearNeedle, f]
    score1 <- myGrep [scoreNeedle, f]
    (score, reviewCount) <- if score1 == ""
      then do
        Just goodN <-
             readMaybe <$> myGrep ["-c", "media-img icon small fresh", f]
        Just badN <-
             readMaybe <$> myGrep ["-c", "media-img icon small rotten", f]
        let total = goodN + badN
        return $ if total == 0 then ("0", "0") else
            (show . round $ 100 * intToD goodN / intToD total, show total)
      else (,) score1 . pullDig <$>
        myGrep ["-oP", "(?:reviewCount\":)[0-9]*", f]
    genre <-
        myHeadStr . map T.unpack . T.splitOn (T.pack ",\"image\"") .
            T.pack .
        myHeadStr . lines <$> myGrep ["-oP", "(?<=genre\":)[^}]*", f]
    return (score, reviewCount, year, genre)

rtInfo isSeries (Title title) = do
    let f = "scrape/rotten-tomatoes" </>
            (if isSeries then "series" else "standalone") </> title
    fExists <- doesFileExist f
    if fExists then Just <$> rtFileInfo f else return Nothing

rtScoreGenres duration title year = do
    rtRes <- rtInfo (isLeft duration) title
    --when (title == Title "the_last_kingdom") $ print rtRes
    case rtRes of
      Just (score, reviewCount, rtYear, rtGenre) ->
        if abs (year - either id (const 0) duration - rtYear) <= 2
          then return (Just (read score, read reviewCount), rtGenre)
          else return (Nothing, "")
      Nothing -> return (Nothing, "")

readDescFile :: String -> IO (String, Desc)
readDescFile f = do
    let filename = "scrape/netflix/descs" </> f <> ".html"
        needle = "class=\"show-title\""
    x1List <- T.splitOn needle <$> T.readFile filename
    when (length x1List < 2) $
        error $ "Could not find in " ++ show filename ++ ": " ++ show needle
    let _:x1:_ = x1List
        x2 = T.tail $ snd $ T.breakOn ">" x1
        (titleEnc, x3) = T.breakOn "<" x2
        _:x4:_ = T.splitOn "class=\"year\"" x3
        x5 = T.tail $ snd $ T.breakOn ">" x4
        yearStr:_ = T.words x5
        _:x6:_ = T.splitOn "class=\"maturity-number\"" x5
        x7 = T.tail $ snd $ T.breakOn ">" x6
        rating:_ = T.words x7
        _:x8:_ = T.splitOn "class=\"duration\"" x7
        x9 = T.tail $ snd $ T.breakOn ">" x8
        x10 = if "<" `T.isPrefixOf` x9
          then T.tail $ snd $ T.breakOn ">" x9
          else x9
        (durationOrSeasons, x11) = T.breakOn "<" x10
        _:x12:_ = T.splitOn "class=\"synopsis\"" x11
        x13 = T.tail $ snd $ T.breakOn ">" x12
        (descriptionEnc, x14) = T.breakOn "<" x13
        (actorsEnc, x17) =
            case T.splitOn "class=\"actors-list\"" x14 of
              _:x:_ -> T.breakOn "<" $ T.tail $ snd $ T.breakOn ">" x
              _ -> ("", x14)
        _:x18:_ = trace f $ T.splitOn "class=\"genre-list\"" x17
        x19 = T.tail $ snd $ T.breakOn ">" x18
        (genresEnc, _x20) = T.breakOn "<" x19
        title = unenc titleEnc
        actors = unenc actorsEnc
        description = unenc descriptionEnc
        genres = unenc genresEnc
        duration = if
            "Season" `T.isInfixOf` durationOrSeasons ||
            "Series" `T.isInfixOf` durationOrSeasons ||
            "Parts" `T.isInfixOf` durationOrSeasons
          then Left (read $ T.unpack $ head $ T.words durationOrSeasons)
          else Right $ T.replace " " "" durationOrSeasons
        year = read $ T.unpack yearStr
    (rt, extraGenres) <-
        rtScoreGenres duration (flattenTitle $ T.unpack title) year
    return (f, Desc title year rating actors description genres duration
        rt (T.pack extraGenres))

hshRun :: (String, [String]) -> IO [String]
hshRun = HSH.run

-- 2018-12-08 ordering of audio lang of unseen avail in US, fewest first
langOrd :: [String]
langOrd =
    [ "ar"
    , "zh"
    , "pt"
    , "jp"
    , "de" -- a lot of things are in all of it,de,fr; we have DW anyway?
    , "it"
    , "fr"
    , "es"
    ]

getLangNetflixNums :: String -> IO (HashSet String)
getLangNetflixNums lang =
    HS.delete "80007017" . -- removed Oct 17
    HS.delete "80173243" . -- test patterns
    HS.delete "80116008" . -- redirect little_baby_bum_nursery_rhyme_friends
    HS.fromList <$> liftM2 (<>)
    (hshRun ("grep", ["-oP", "(?<=https://www.netflix.com/watch/)[0-9]*",
        "scrape/netflix/lists" </> lang <> ".html"]))
    (lines <$> readFile ("scrape/netflix/unogs.com" </> lang <> ".txt"))

getAllNetflixNums :: IO (HashSet String)
getAllNetflixNums = HS.unions <$> mapM getLangNetflixNums langOrd

loadList :: IO [(String, ([String], Desc))]
loadList = do
    Right resMap <- Store.decode <$> B.readFile "results.data"
    return $ HM.toList resMap

shrinkGenres =
    T.replace "," "" .
    T.replace " " "" .
    T.replace "-" "" .
    T.replace "&" "" .
    T.replace "'" ""

type NumLangDesc = (String, ([String], Desc))

showNumLangDesc :: NumLangDesc -> [Text]
showNumLangDesc (n, (langs, Desc title year rating actors description genres
                                  duration rt extraGenres)) =
    [ "- " <> T.pack (show year) <> " " <> T.pack scoreStr <> " " <>
        title <> " " <> rating <>
        " " <> either ((<> "S") . T.pack . show) id duration <> " " <>
        T.intercalate "," (map T.pack langs)
    , "  " <> T.pack n <> " " <> T.pack flatTitle <> " " <> actors
    , "  " <> description
    , "  " <> shrinkGenres genres -- <> extraGenres
    , ""
    ]
  where
    flatTitle = unTitle $ flattenTitle $ T.unpack title
    scoreStr = maybe "?%/?" (\(s, c) -> show s <> "%/" <> show c) rt

