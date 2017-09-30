import Data.Char
import Data.List
import Data.Ord
import HSH
import System.Environment
import System.Exit
import System.Process

import FlattenTitle

runStrCode :: (String, [String]) -> IO (String, ExitCode)
runStrCode = run

myRun :: (String, [String]) -> IO String
myRun = fmap fst . runStrCode

pullDig = filter isDigit

data Info = Info
    { iWeightedScore  :: Double
    , iTomPercent     :: Maybe Int
    , iTomReviewCount :: Maybe Int
    , iCertFreshStr   :: String
    , iYearStr        :: String
    , iRatingStr      :: String
    , iGenreStr       :: String
    , iNameStr        :: String
    , iIsSerial       :: Bool
    }
    deriving (Eq, Ord)

showInfo :: Info -> String
showInfo (Info f score reviewCount certifiedFresh year rating genre name isS) =
    if isS
      then
        show (round f :: Int) ++ " " ++
        "[" ++ year ++ "] " ++
        maybe "?" show score ++ "%/" ++ maybe "?" show reviewCount ++
        (if certifiedFresh == "1" then "*" else "") ++
        " Series:" ++ name
      else
        show (round f :: Int) ++ " " ++
        "[" ++ year ++ "] " ++
        maybe "?" show score ++ "%/" ++ maybe "?" show reviewCount ++
        (if certifiedFresh == "1" then "*" else "") ++
        " " ++ rating ++
        " " ++ name ++
        " " ++ genre

pullInfo :: Bool -> String -> IO Info
pullInfo isSeries file =
    if isSeries then do
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
        let rating = "?"
        let genre = "?"
        return $
            Info 0
            (if score == "" then Nothing else Just $ read score)
            (if reviewCount == "" then Nothing else Just $ read reviewCount)
            certifiedFresh year rating genre file isSeries
    else do
        (_,out,_) <- readProcessWithExitCode "grep"
            ["mpscall\\[\"cag\\[score", "m/" ++ file] ""
        let score = pullDig out

        (_,out,_) <- readProcessWithExitCode "grep"
            ["mpscall\\[\"cag\\[certified_fresh", "m/" ++ file] ""
        let certifiedFresh = take 1 $ pullDig out

        (_,out,_) <- readProcessWithExitCode "grep"
            ["-oP", "(?:reviewCount\":)[0-9]*", "m/" ++ file] ""
        let reviewCount = pullDig out

        (_,out,_) <- readProcessWithExitCode "grep"
            ["mpscall\\[\"cag\\[release", "m/" ++ file] ""
        let year = pullDig out

        (_,out,_) <- readProcessWithExitCode "grep"
            ["-oP", "(?<=rating]\"]=\")[^\"]*", "m/" ++ file] ""
        let rating = map toUpper $ init out

        (_,out,_) <- readProcessWithExitCode "grep"
            ["-oP", "(?<=genre\":)[^}]*", "m/" ++ file] ""
        let genre = head $ lines out
        return $
            Info 0
            (if score == "" then Nothing else Just $ read score)
            (if reviewCount == "" then Nothing else Just $ read reviewCount)
            certifiedFresh year rating genre file isSeries

decSort :: Ord b => (a -> b) -> [a] -> [a]
decSort f xs = map snd $ sortBy (comparing fst) [(f x, x) | x <- xs]

calcInfoScore :: Info -> Info
calcInfoScore (Info _ (Just score) (Just reviewCount) a b c d e f) =
    Info
    -- (fromIntegral score * (1 + log (fromIntegral reviewCount)))
    (fromIntegral score)
    (Just score) (Just reviewCount) a b c d e f

isGreatInfo :: Info -> Bool
isGreatInfo (Info _ (Just score) (Just _) _ _ _ _ _ _) = score >= 75
isGreatInfo _ = False

main :: IO ()
main = do
    [f, seenF] <- getArgs
    mustBeIn <- fmap (map flattenTitle . lines) $ readFile f 
    seens <- fmap (map flattenTitle . lines) $ readFile seenF
    let myFilter = filter (\x -> x `elem` mustBeIn && x `notElem` seens)
    movieFiles <- fmap myFilter $ run ("ls", ["m"])
    infos1 <- mapM (pullInfo False) movieFiles
    seriesFiles <- fmap myFilter $ run ("ls", ["tv"])
    infos2 <- mapM (pullInfo True) seriesFiles
    mapM_ (putStrLn . showInfo) $ sortBy (flip compare) $ map calcInfoScore $
         filter isGreatInfo $ infos1 ++ infos2
