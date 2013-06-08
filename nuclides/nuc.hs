#include <h>

data Nuc = Nuc {
  nucAtomicN :: Int,
  nucNeutronN :: Int,
  nucHalflife :: Halflife
  }
  deriving (Eq, Ord, Show)

data Halflife = HlInf | HlBeyondObs | HlFin Float
  deriving (Eq, Ord, Show)

readNuc l =
  Nuc atomicN neutronN (readHalflife . takeWhile (not . isSpace) $ ws !! 9)
  where
  ws = splitOn "\t" l
  atomicN = (read $ ws !! 2)
  neutronN = (read $ ws !! 3)
  readHalflife x = if x == "infinity" 
    then if hlIsActuallyInf atomicN neutronN then HlInf else HlBeyondObs
    else HlFin $ read x

hlInfAtomicNToMasses = [
  (1,[1,2]),
  (2,[3,4]),
  (3,[6,7]),
  (4,[9]),
  (5,[10,11]),
  (6,[12,13]),
  (7,[14,15]),
  (8,[16..18]),
  (9,[19]),
  (10,[20..22]),
  (11,[23]),
  (12,[24..26]),
  (13,[27]),
  (14,[28..30]),
  (15,[31]),
  (16,[32,33,34,36]),
  (17,[35,37]),
  (18,[38,40]),
  (19,[39,41]),
  (20,[42..44]),
  (21,[45]),
  (22,[46..50]),
  (23,[51]),
  (24,[52..54]),
  (25,[55]),
  (26,[56..58]),
  (27,[59]),
  (28,[60,61,62,64]),
  (29,[63,65]),
  (30,[66..68]),
  (31,[69,71]),
  (32,[70,72,73,74]),
  (33,[75]),
  (34,[76..78]),
  (35,[79,81]),
  (36,[80,82,83,84]),
  (37,[85]),
  (38,[86..88]),
  (39,[89]),
  (40,[90..92])
  ]

hlIsActuallyInf atomicN neutronN =
  case lookup atomicN hlInfAtomicNToMasses of
    Nothing -> False
    Just masses -> any (== atomicN + neutronN) masses

loadElemInfo = do
  lines <$> readFile "/home/danl/n/s/chem/elements"

main = do
  nucs <- nub . map readNuc . drop 1 . lines <$> readFile "nuclides"
  putStr . unlines $ map show nucs
