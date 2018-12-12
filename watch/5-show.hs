#include <h>

import Share
import Secret

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
    doOutTypes res expiringIds
