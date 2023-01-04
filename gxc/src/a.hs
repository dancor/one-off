-- Gen .XCompose
-- Includes fail-out functionality where partial codes are sent
-- pinyin 3caron; esperanto u(breve <circumflex
import qualified Data.IntMap as IM
import Data.Char (chr, ord, toUpper)
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
type Ch = Int -- store characters by their ord
data Key = Key {_kName :: S, _kCh :: Ch}; ck c = Key [c] (ord c)
data Tree = Tree {untree :: IM Tree} deriving Show
type IM = IM.IntMap
type S = String
type Code = S -- Last Char is the result of typing in the Chars before it
lol :: Tree -> IO ()
lol t = lol2 0 t
lol2 :: Int -> Tree -> IO ()
lol2 d (Tree m) = mapM_ (lol3 d) $ IM.toList m
lol3 :: Int -> (Int, Tree) -> IO ()
lol3 d (i, t) = putStrLn (replicate d '-' ++ [chr i]) >> lol2 (d + 1) t
lowCap :: Char -> S -> S -> S -> [Code]
lowCap mod ltrs lows caps = zipWith (\l c -> [mod, l, c]) ltrs lows ++
  zipWith (\l c -> [mod, toUpper l, c]) ltrs caps
symbKeys :: [Key]
symbKeys = Key "space" (ord ' ') : (map (\[s, n] -> Key n (ord $ head s)) $
  chunksOf 2 $ words $
  "` grave - minus = equal ; semicolon [ bracketleft ] bracketright " ++ 
  "\\ backslash , comma . period / slash " ++
  "~ asciitilde ! exclam @ at # numbersign $ dollar % percent " ++
  "^ asciicircum & ampersand * asterisk ( parenleft ) parenright " ++
  "_ underscore + plus : colon { braceleft } braceright | bar \" quotedbl " ++
  "< less > greated ? question")
normKeys :: [Key]
normKeys = Key trig trigI : symbKeys ++ 
  map ck (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])
-- At each node, complete partial codes for all normKeys
ordNameM :: IM S
ordNameM = IM.fromList $ map (\(Key name o) -> (o, name)) normKeys
o2n i = case IM.lookup i ordNameM of Just x -> x; _ -> [chr i]
codeTr :: Code -> Tree
codeTr [] = Tree IM.empty
codeTr (c:cR) = Tree $ IM.singleton (ord c) $ codeTr cR
trUnion :: Tree -> Tree -> Tree
trUnion (Tree a) (Tree b) = Tree (IM.unionWith trUnion a b)
trUnions :: [Tree] -> Tree
trUnions = Tree . IM.unionsWith trUnion . map untree
trig :: S
trig = "F13"
trigI :: Int
trigI = 39
ang :: S -> S; ang x = '<':x++">"
gen :: [Int] -> Tree -> [S]
gen pre (Tree m) =
  (if isSingEnd m then [] else
  --[ render (pre ++ [k]) (map chr $ pre ++ [k]) ++ show m
  [ render (pre ++ [k]) (map chr $ pre ++ [k])
  | Key _ k <- normKeys, IM.notMember k m]) ++ concatMap (genB pre) (IM.toList m)
render give get = concatMap (ang . o2n) give ++ ":\"" ++ f get ++ "\"" where
  f ('\\':r) = "\\\\" ++ f r; f ('"':r) = "\\\"" ++ f r; f (x:r) = x : f r
  f [] = ""
isSingEnd :: IM Tree -> Bool
isSingEnd = f . IM.elems where f [Tree m] = IM.null m; f _ = False
genB :: [Int] -> (Int, Tree) -> [S]
genB pre (i, Tree m) = if IM.null m then [render pre [chr i]]
  else 
    {- (if isSingEnd m then [] else
    [ render (pre ++ [i, k]) (map chr $ pre ++ [i, k])
    --[ render (pre ++ [i, k]) (map chr $ pre ++ [i, k]) ++ show m
    | Key _ k <- normKeys, IM.notMember k m]
  ) ++ -} gen (pre ++ [i]) (Tree m)
main :: IO ()
main = do
  let t = trUnions $ map codeTr $ -- take 16 $
        lowCap '1' "aeiouv" "āēīōūǖ" "ĀĒĪŌŪǕ" ++
        lowCap '2' "aceinosuvz" "áćéíńóśúǘź" "ÁĆÉÍÓŃŚÚǗŹ" ++
        lowCap '3' "aeiouv" "ǎěǐǒǔǚ" "ǍĚǏǑǓǙ" ++
        lowCap '4' "aeiouv" "àèìòùǜ" "ÀÈÌÒÙǛ" ++
        lowCap '6' "aceghijosuyz" "âĉêĝĥîĵôŝûŷẑ" "ÂĈÊĜĤÎĴÔŜÛŶẐ"  ++
        lowCap ';' "aeiou" "äëïöü" "ÄËÏÖÜ" ++
        lowCap '9' "u" "ŭ" "Ŭ" ++
        lowCap 'o' "a" "å" "Å" ++
        lowCap 'c' "c" "ç" "Ç" ++
        lowCap 'n' "ano" "ãñõ" "ÃÑÕ" ++
        lowCap 's' "s" "ß" "ẞ" ++
        lowCap '/' "lo" "łø" "ŁØ" ++
        lowCap '.' "z" "ż" "Ż" ++
        lowCap ',' "ae" "ąę" "ĄĘ" ++
        [(["h"],"ʻ")] ++
        [(["a","e"],"æ")] ++ [(["A","e"],"Æ")] ++
        [(["o","e"],"œ")] ++ [(["O","e"],"Œ")]
  mapM_ putStrLn $ "<F13><F13>:\"'\"" : tail (gen [trigI] t)
