#include <h>

procLine [num, _, sent] = (num, sent)
procLine x = error $ "procLine: " ++ show x

splitCols = DT.splitOn "\t"

readNumToSentMap filename = 
    HMS.fromList . map (procLine . splitCols) .
    DT.lines <$> DTI.readFile filename

main = do
    tatDir <- (\x -> x </> "data" </> "tatoeba") <$> getHomeDirectory
    l1 <- readNumToSentMap $ tatDir </> "simp-cmn.csv"
    l2 <- readNumToSentMap $ tatDir </> "eng.csv"
    nums <- HMS.fromListWith (++) . map (\[a, b] -> (a, [b])) .
        filter (\[a, b] -> a `HMS.member` l1 && b `HMS.member` l2) .
        map splitCols . DT.lines . DTE.decodeUtf8 <$>
        HSH.run ("xzcat" :: String, ["links.csv.xz" :: String]) 
    let entries = [
            [ n1
            , fromJust (HMS.lookup n1 l1)
            , maximumBy (compare `on` DT.length)
              [fromJust (HMS.lookup n2 l2) | n2 <- n2s]
            ] | (n1, n2s) <- HMS.toList nums]
    BSLC.putStrLn $ "var entries = " <> Ae.encode entries <> ";"
