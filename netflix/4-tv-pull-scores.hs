tvshowInfo :: Info -> String
showInfo (Info score reviewCount certifiedFresh year rating genre name) =
    "[" ++ year ++ "] " ++
    maybe "?" show score ++ "%/" ++ maybe "?" show reviewCount ++
    (if certifiedFresh == "1" then "*" else "") ++
    -- " " ++ rating ++
    " Series:" ++ name ++
    -- " " ++ genre ++
    ""

pullInfo :: String -> IO Info
pullInfo file = do
    (_,out,_) <- readProcessWithExitCode "grep"
        ["-oP", "(?<=ratingValue\":)[^}]*", "tv/" ++ file] ""
    let score = out

    (_,out,_) <- readProcessWithExitCode "grep"
        ["mpscall\\[\"cag\\[certified_fresh", "tv/" ++ file] ""
    let certifiedFresh = take 1 $ pullDig out

    (_,out,_) <- readProcessWithExitCode "grep"
        ["-oP", "(?:reviewCount\":)[0-9]*", "tv/" ++ file] ""
    let reviewCount = pullDig out

    (_,out,_) <- readProcessWithExitCode "grep"
        ["mpscall\\[\"cag\\[release", "tv/" ++ file] ""
    let year = pullDig out

    {-
    (_,out,_) <- readProcessWithExitCode "grep"
        ["-oP", "(?<=rating]\"]=\")[^\"]*", "tv/" ++ file] ""
    let rating = map toUpper $ init out
    -}
    let rating = "?"

    {-
    (_,out,_) <- readProcessWithExitCode "grep"
        ["-oP", "(?<=genre\":)[^}]*", "tv/" ++ file] ""
    let genre = head $ lines out
    -}
    let genre = "?"

    return $
        Info
        (if score == "" then Nothing else Just $ read score)
        (if reviewCount == "" then Nothing else Just $ read reviewCount)
        certifiedFresh year rating genre file

decSort :: Ord b => (a -> b) -> [a] -> [a]
decSort f xs = map snd $ sortBy (comparing fst) [(f x, x) | x <- xs]

myInfoScore :: Info -> Double
myInfoScore (Info (Just score) (Just reviewCount) _ _ _ _ _) =
    -1 * fromIntegral score * (1 + log (fromIntegral reviewCount))

isGreatInfo :: Info -> Bool
isGreatInfo (Info (Just score) (Just _) _ _ _ _ _) = score >= 90
isGreatInfo _ = False

main :: IO ()
main = do
    files <- run ("ls", ["tv"])
    infos <- mapM pullInfo files
    mapM_ (putStrLn . showInfo) $ decSort myInfoScore $
         filter isGreatInfo infos
