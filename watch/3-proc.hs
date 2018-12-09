#include <h>

import qualified Data.Store as Store

import Share

doLang :: String -> IO (HashMap String [String])
doLang lang = do
    nums <- HS.toList <$> getLangNetflixNums lang
    return $ HM.fromList [(n, [lang]) | n <- nums]

hmsUnionsWith :: (Hashable k, Eq k, Foldable t) => (v -> v -> v)
    -> t (HM.HashMap k v) -> HM.HashMap k v
hmsUnionsWith f = foldl' (HM.unionWith f) HM.empty

readDescs :: IO (HashMap String Desc)
readDescs = do
    (finStandalone, finSeries) <- readFinished
    nums <- HS.toList <$> getAllNetflixNums
    let hasMore (_, d) =
            let tit = unTitle (flattenTitle $ T.unpack $ dTitle d) in
            case dDuration d of
              Left numSeasonsSeen -> case HM.lookup tit finSeries of
                Just numSeasonsAvail -> numSeasonsSeen > numSeasonsAvail
                Nothing -> True
              Right _ -> not $ tit `HS.member` finStandalone
    HM.fromList . filter hasMore <$> mapM readDescFile nums

readFinished :: IO (HashSet String, HashMap String Int)
readFinished = do
    let clean = takeWhile (/= '#')
        procSeason ('S':s) =
            let Just sInt = readMaybe $ takeWhile isDigit s
            in if "E" `isInfixOf` s then sInt - 1 else sInt
    standalone <- HS.fromList . map (head . words . clean) . lines <$>
        readFile "finished"
    series <- HM.fromList .
        map (\[name, season] -> (name, procSeason season)) .
        filter ((>= 2) . length) . map (words . clean) . lines <$>
        readFile "started"
    return (standalone, series)

main :: IO ()
main = do
    numToLang <- hmsUnionsWith (++) <$> mapM doLang langOrd
    numToDesc <- readDescs
    B.writeFile "results.data" $ Store.encode $
        HM.intersectionWith (,) numToLang numToDesc
