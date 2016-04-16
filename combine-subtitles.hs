#include <h>

subHeader = DT.unlines
  [ "[INFORMATION]"
  , "[AUTHOR]"
  , "[SOURCE]"
  , "[PRG]"
  , "[FILEPATH]"
  , "[DELAY]"
  , "[CD TRACK]"
  , "[COMMENT]"
  , "[END INFORMATION]"
  , "[SUBTITLE]"
  , "[COLF]&HFFFFFF,[STYLE]no,[SIZE]18,[FONT]Arial"
  ]

data SubLine
  = SubLine
  { sStart :: Rational
  , sEnd   :: Rational
  , sLine  :: DT.Text
  }

readSTime str = 3600 * h + 60 * m + s
  where
    [h, m, s] = map (d2r . read . DT.unpack) $ DT.split (== ':') str

readSub [] = []
readSub ("":rest) = readSub rest
readSub (times:l:rest) = SubLine (readSTime s) (readSTime e) l : readSub rest
  where
    (s, e) = case DT.split (== ',') times of
      [s, e] -> (s, e)
      hm -> error $ show hm

readSrt [] = []
readSrt (_:times:linesAndRest) =
    SubLine (readSTime s) (readSTime e) (DT.intercalate "[br]" lines) :
    readSrt rest
  where
    [s, e] = map (DT.replace "," ".") $ DT.splitOn " --> " times
    (lines, spAndRest) = break (== "") linesAndRest
    rest = drop 1 spAndRest

showSub ls = subHeader <> DT.intercalate "\n" (map showSubLine ls)

floori :: Rational -> Int
floori = floor

r2d :: Rational -> Double
r2d = realToFrac

d2r :: Double -> Rational
d2r = realToFrac

showSTime t = DT.pack (printf "%02d" . floori $ t / 3600) <> ":" <>
    DT.pack (printf "%02d" $ floori (t / 60) `mod` 60) <> ":" <>
    DT.pack (printf "%02d" $ floori t `mod` 60) <>
    DT.pack (tail . printf "%.2f" . r2d $ t `mod'` 1)

showSubLine (SubLine s e l) =
    showSTime s <> "," <> showSTime e <> "\n" <> l <> "\n"

combineOverlap (SubLine s1 e1 l1 : SubLine s2 e2 l2 : rest) = if e1 > s2
  then SubLine s1 e2 (l1 <> "[br]" <> l2) : combineOverlap rest
  else SubLine s1 e1 l1 : combineOverlap (SubLine s2 e2 l2 : rest)
combineOverlap xs = xs

killOverlap (SubLine s1 e1 l1 : SubLine s2 e2 l2 : rest) =
    SubLine s1 (min e1 s2) l1 : killOverlap (SubLine s2 e2 l2 : rest)
killOverlap xs = xs

main = do
    en <- readSub . dropWhile (not . ("0" `DT.isPrefixOf`)) . DT.lines <$>
        DTI.readFile "en.sub"
    de <- readSrt . DT.lines <$> DTI.readFile "de.srt"
    DTI.putStr $ showSub $ killOverlap $ combineOverlap $
        sortBy (compare `on` sStart) $ en <> de
