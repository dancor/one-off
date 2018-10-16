#include <h>

--import Title

-- 2018-10-15 ordering by total titles avail in US w/ audio, fewest first
langOrd :: [String]
langOrd = 
    [ "jp"
    , "ar"
    , "pt"
    , "zh"
    , "de"
    , "fr"
    , "es"
    ]

doLang :: String -> IO (HMS.HashMap String [String])
doLang lang = do
    nfNums <- hshRun ("grep", ["-oP",
        "(?<=https://www.netflix.com/watch/)[0-9]*", "txt" </> lang <> ".html"]
        )
    return $ HMS.fromList [(n, [lang]) | n <- nfNums]

hmsUnionsWith :: (Hashable k, Eq k, Foldable t) => (v -> v -> v)
    -> t (HMS.HashMap k v) -> HMS.HashMap k v
hmsUnionsWith f = foldl' (HMS.unionWith f) HMS.empty

data Desc = Desc
  { dTitle :: DT.Text
  , dActors :: DT.Text
  , dDescription :: DT.Text
  , dGenres :: DT.Text
  , dDuration :: Either Int DT.Text
  } deriving Show

unenc :: DT.Text -> DT.Text
unenc =
    DT.replace "&amp;" "&" .
    DT.replace "&quot;" "\"" .
    DT.replace "&#x27;" "'"

readDescFile :: String -> IO (String, Desc)
readDescFile f = do
    _:x1:_ <-
        DT.splitOn "class=\"show-title\" data-reactid=\"" <$>
        DTI.readFile ("descFiles" </> f)
    let x2 = DT.tail $ snd $ DT.breakOn ">" x1
        (titleEnc, x3) = DT.breakOn "<" x2
        _:x4:_ = DT.splitOn "class=\"year\" data-reactid=\"" x3
        x5 = DT.tail $ snd $ DT.breakOn ">" x4
        year:_ = DT.words x5
        _:x6:_ = DT.splitOn "class=\"maturity-number\" data-reactid=\"" x5
        x7 = DT.tail $ snd $ DT.breakOn ">" x6
        rating:_ = DT.words x7
        _:x8:_ = DT.splitOn "class=\"duration\" data-reactid=\"" x7
        x9 = DT.tail $ snd $ DT.breakOn ">" x8
        x10 = if "<" `DT.isPrefixOf` x9
          then DT.tail $ snd $ DT.breakOn ">" x9
          else x9
        (durationOrSeasons, x11) = DT.breakOn "<" x10
        _:x12:_ = DT.splitOn "class=\"synopsis\" data-reactid=\"" x11
        x13 = DT.tail $ snd $ DT.breakOn ">" x12
        (descriptionEnc, x14) = DT.breakOn "<" x13
        (actorsEnc, x17) =
            case DT.splitOn "class=\"actors-list\" data-reactid=\"" x14 of
              _:x:_ -> DT.breakOn "<" $ DT.tail $ snd $ DT.breakOn ">" x
              _ -> ("", x14)
        _:x18:_ = DT.splitOn "class=\"genre-list\" data-reactid=\"" x17
        x19 = DT.tail $ snd $ DT.breakOn ">" x18
        (genresEnc, _x20) = DT.breakOn "<" x19
        title = unenc titleEnc
        actors = unenc actorsEnc
        description = unenc descriptionEnc
        genres = unenc genresEnc
        duration = if "Season" `DT.isInfixOf` durationOrSeasons
          then Left (read $ DT.unpack $ head $ DT.words durationOrSeasons)
          else Right $ DT.replace " " "" durationOrSeasons
    return (f, Desc title actors description genres duration)

readDescs :: IO (HMS.HashMap String Desc)
readDescs = do
    fs <- hshRun ("ls", ["descFiles"])
    HMS.fromList <$> mapM readDescFile fs

printNumLangDesc :: (String, ([String], Desc)) -> IO ()
printNumLangDesc
        (n, (langs, Desc title actors description genres duration)) = do
    DTI.putStrLn $ title <> " " <>
        either ((<> "S") . DT.pack . show) id duration <> " " <>
        DT.intercalate "," (map DT.pack langs)
    DTI.putStrLn $ "- " <> actors
    DTI.putStrLn $ "- " <> description
    DTI.putStrLn $ "- " <> genres
    DTI.putStrLn ""

readFinished :: IO (HSet.HashSet String, HMS.HashMap String Int)
readFinished = do
    let clean = takeWhile (/= '#')
        procSeason ('S':s) = if "E" `isInfixOf` s then read s - 1 else read s
    standalone <- HSet.fromList . map (head . words . clean) . lines <$>
        readFile "finished"
    series <- HMS.fromList . 
        map (\[name, season] -> (name, procSeason season)) . 
        filter ((>= 2) . length) . map (words . clean) . lines <$>
        readFile "started"
    return (standalone, series)

main :: IO ()
main = do
    numToLang <- hmsUnionsWith (++) <$> mapM doLang langOrd
    numToDesc <- readDescs
    let res = HMS.intersectionWith (,) numToLang numToDesc
    --(finStandalone, finSeries) <- readFinished
    print $ HMS.size res
    putStrLn ""
    mapM_ printNumLangDesc $ HMS.toList res
