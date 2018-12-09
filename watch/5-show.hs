#include <h>

import Share
import Secret

hasOfLangs needles l = any (`elem` l) needles

isOfLangs needles l = any (== head l) needles

kidRank d = if | "forages0to2"   `T.isInfixOf` g -> 0
               | "forages3to4"   `T.isInfixOf` g -> 3
               | "forages5to7"   `T.isInfixOf` g -> 5
               | "forages8to10"  `T.isInfixOf` g -> 8
               | "forages11to12" `T.isInfixOf` g -> 11
               | otherwise                        -> 13
  where
    g = shrinkGenres $ dGenres d

resFilt res f = sortBy (
    (compare `on` kidRank . snd . snd) <>
    (flip compare `on` dRt . snd . snd)) $ filter f res

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (x:xs) = f x  : xs

doFile filename ls = do
    print filename
    T.writeFile filename $ T.unlines ls

printFile
    :: [NumLangDesc]
    -> String
    -> (NumLangDesc -> Bool)
    -> IO ()
printFile res filename f =
    doFile filename $ concatMap showNumLangDesc $ resFilt res f

procDay :: [String] -> [(String, Text)]
procDay [] = []
procDay (day:ls) =
    let dayStr = T.pack $
            ((\[m,d] -> map toLower (take 3 m) <> d) . take 2 . words .
            takeWhile (/= '<') . tail . dropWhile (/= '>') . tail $
            dropWhile (/= '>') day) <> " "
    in map (flip (,) dayStr . takeWhile isDigit) $ concat
        [catMaybes $ map (stripPrefix "www.netflix.com/title/") (tails l)
        | l <- ls]

expFile res expiringIds filename f = doFile filename .
    concatMap (\(expDate,x) -> onHead (expDate <>) $ showNumLangDesc x) .
    sortBy (compare `on` fst) .
    map (\x@(n,_) -> (fromJust $ HM.lookup n expiringIds, x)) $
    filter (\x@(n,_) -> n `HM.member` expiringIds && f x) res

main :: IO ()
main = do
    Right resMap <- Store.decode <$> B.readFile "results.data"
    dayBlocks <-
        Spl.splitWhen (== "<span style=\"font-size: large;\"><br /></span>") .
        takeWhile (not . null) .
        dropWhile (not . ("Today is the last day to watch" `isInfixOf`)) .
        lines <$> run
        ("grep -v '<span style=\"color: red;.*Renewed' index.html" :: String)
    let res = HM.toList resMap
        expiringIds = HM.fromList $ concatMap procDay dayBlocks
    printFile res "out-all" (const True)
    expFile res expiringIds "out-exp" (const True)
    expFile res expiringIds "out-exp-ez" (\(_,(l,_)) -> isOfLangs ezLangs l)
    expFile res expiringIds "out-exp-hard-anime" (\(_,(l,d)) ->
        not (isOfLangs ezLangs l) && "Anime" `T.isInfixOf` dGenres d)
    expFile res expiringIds "out-exp-hard-nonanime" (\(_,(l,d)) ->
        not $ isOfLangs ezLangs l || "Anime" `T.isInfixOf` dGenres d)
    printFile res "out-ez-me"
        (\(_,(l,d)) -> isOfLangs ezLangs l && kidRank d == 13 &&
            ( "LGBT" `T.isInfixOf` (dGenres d) ||
              "SciFi" `T.isInfixOf` shrinkGenres (dGenres d)
            ))
    printFile res "out-is-paul" (isOfLangs paulLangs . fst . snd)
    printFile res "out-has-paul" (hasOfLangs paulLangs . fst . snd)
    printFile res "out-lgbt" (("LGBT" `T.isInfixOf`) . dGenres . snd . snd)
    printFile res "out-is-paul-lgbt" (\(_,(l,d)) -> 
        isOfLangs paulLangs l && ("LGBT" `T.isInfixOf`) (dGenres d))
    printFile res "out-has-paul-lgbt" (\(_,(l,d)) -> 
        hasOfLangs paulLangs l && ("LGBT" `T.isInfixOf`) (dGenres d))
    mapM_ (\lang ->
        printFile res ("out-is-" <> lang) (isOfLangs [lang] . fst . snd))
        langOrd
    mapM_ (\lang ->
        printFile res ("out-has-" <> lang) (hasOfLangs [lang] . fst . snd))
        ["zh"]
