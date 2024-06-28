#include <h>
type T = Text
val :: T -> Rational
val t =
  if t=="Aaa" ||t=="AAA" then 100 else
  if t=="Aa1" ||t=="AA+" then 100*22/23 else
  if t=="Aa2" ||t=="AA"  then 100*21/23 else
  if t=="Aa3" ||t=="AA-" then 100*20/23 else
  if t=="A1"  ||t=="A+"  then 100*19/23 else
  if t=="A2"  ||t=="A"   then 100*18/23 else
  if t=="A3"  ||t=="A-"  then 100*17/23 else
  if t=="Baa1"||t=="BBB+"then 100*16/23 else
  if t=="Baa2"||t=="BBB" then 100*15/23 else
  if t=="Baa3"||t=="BBB-"then 100*14/23 else
  if t=="Ba1" ||t=="BB+" then 100*13/23 else
  if t=="Ba2" ||t=="BB"  then 100*12/23 else
  if t=="Ba3" ||t=="BB-" then 100*11/23 else
  if t=="B1"  ||t=="B+"  then 100*10/23 else
  if t=="B2"  ||t=="B"   then 100* 9/23 else
  if t=="B3"  ||t=="B-"  then 100* 8/23 else
  if t=="Caa1"||t=="CCC+"then 100* 7/23 else
  if t=="Caa2"||t=="CCC" then 100* 6/23 else
  if t=="Caa3"||t=="CCC-"then 100* 5/23 else
  if t=="Ca"  ||t=="CC"  then 100* 4/23 else
  if t=="Ca2" ||t=="C"   then 100* 3/23 else
  error $ show t
procLine l = let
  name:snp:moo:fit:_ = map (T.dropWhileEnd (== ' ')) $ T.splitOn "\t" l in
  case (snp,moo,fit) of
    ("",_,_) -> Nothing
    ("NR",_,_) -> Nothing
    ("SD",_,_) -> Nothing
    (_,"",_) -> Nothing
    (_,"WR",_) -> Nothing
    (_,_,"") -> Nothing
    _ -> Just (round $ (val snp + val moo + val fit) / 3, name)
main = do
  ls <- drop 1 . T.lines <$> T.readFile "cred.txt"
  let list = sortBy (comparing (\(a, b) -> (-a, b))) . catMaybes $
        map procLine ls
  print $ length list
  mapM_ (\xs -> T.putStrLn $ T.pack (show . fst $ head xs) <> "% " <>
    T.intercalate " " (map snd xs)) $ groupBy ((==)`on`fst) list
