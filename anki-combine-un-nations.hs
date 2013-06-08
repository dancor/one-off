#include <h>

rl = take 2 . DT.split (== '\t')

rl2 = DT.splitOn (": ") . DT.drop 1 . DT.dropWhile (/= ' ')

mapify = M.fromList . map (\ [a, b] -> (a, b))

cityCombine (Just a, Just b) =
  --if a == b then a else a `DT.append` " (" `DT.append` b `DT.append` ")"
  if a == b then a else a `DT.append` " (" `DT.append` b `DT.append` ")"
cityCombine (a, b) = 
  fromMaybe "XXX " a `DT.append` " " `DT.append` fromMaybe "XXX" b

pairwise :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
pairwise f (a1, b1) (a2, b2) = (f a1 a2, f b1 b2)

lalala :: (Maybe a, Maybe a) -> (Maybe a, Maybe a) -> (Maybe a, Maybe a)
lalala = pairwise mplus

main = do
  ls <- map rl . DT.lines <$> DTI.readFile "ankiun.txt"
  ls2 <- map rl2 . DT.lines <$> DTI.readFile "p/demog_urban_areas/out.un1"
  let
    lsAll = M.toList . M.map cityCombine $ M.unionWith lalala
      (M.map (\ x -> (Just x, Nothing)) $ mapify ls)
      (M.map (\ x -> (Nothing, Just x)) $ mapify ls2)
  DTI.putStr . DT.unlines $ 
    map (\ (k, v) -> k `DT.append` ";" `DT.append` v) lsAll
