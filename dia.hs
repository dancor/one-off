dia :: [[a]] -> [[a]]
dia [[]] = [[]]
dia (topRow:restRows) = zipWith (:) diaLeft $ [[]] ++ diaInner ++ [[]] where
  diaLeft = reverse topRow ++ restHeads
  diaInner = dia restTails
  (restHeads, restTails) = unzip $ map (\ (x:xs) -> (x, xs)) restRows

p :: [String] -> IO ()
p = putStr . unlines . (++ [""])

main :: IO ()
main = do
  p $ dia [
    "HE",
    "WO"
    ]
  p $ dia [
    "HELLO",
    "WORLD",
    "EIRCH",
    "EOIEO",
    "ZYTNH"
    ]
