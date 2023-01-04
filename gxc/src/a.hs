#!/usr/local/bin/rhs
import qualified Data.IntMap as IM
import Data.Char (chr, ord, toUpper)
import Data.List (foldl')
import Data.List.Split (chunksOf)
type Ch = Int -- store characters by their ord
data Key = Key {_kName :: S, _kCh :: Ch}; ck c = Key [c] (ord c)
data Tree = Node (IM Tree) deriving Show
type IM = IM.IntMap
type S = String
type Tr = Tree
type Code = S -- Last Char is the result of typing in the Chars before it
lol :: Tr -> IO ()
lol t = lol2 0 t
lol2 :: Int -> Tr -> IO ()
lol2 d (Node m) = mapM_ (lol3 d) $ IM.toList m
lol3 :: Int -> (Int, Tr) -> IO ()
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
normKeys = symbKeys ++ map ck (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])
-- At each node, complete partial codes for all normKeys
addCode :: Code -> Tr -> Tr
addCode [] t = t
addCode (c:cR) (Node m) = Node $ IM.insertWith (\_ t -> addCode cR t) (ord c) (Node IM.empty) m
main :: IO ()
main = do
  let codes = 
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
        lowCap ',' "ae" "ąę" "ĄĘ" {- ++
        [(["h"],"ʻ")] ++
        [(["a","e"],"æ")] ++ [(["A","e"],"Æ")] ++
        [(["o","e"],"œ")] ++ [(["O","e"],"Œ")] ++ -}
  print codes
  lol codes

{-
type S = String
trig :: S
trig = "F13"
codes :: IM
codes = [Node "1" [Node "a" [Node "ā" []]]]
gen :: [S] -> [Tree S] -> [S]
gen pre ts = 
  map (\k -> ([kName k], "'" ++ kS k)) 
main :: IO ()
main = do
  mapM_ print codes
  gen [trig] codes
-- Gen .XCompose
-- Includes some fail-out functionality where partial codes can be sent.
-- pinyin 3caron; esperanto u(breve <circumflex
import Control.Arrow
import Data.Char (toUpper)
import Data.List.Split (chunksOf)
keyC = kk 'c'; keyN = kk 'n'; key1 = kk '1'; key2 = kk '2'; key3 = kk '3'
key4 = kk '4'; key9 = kk '9'
keyComma = Key "comma" ","
lowCap mod ltrs lows caps = 
  zipWith (\l c -> ([mod, [l]], [c])) ltrs lows ++
  zipWith (\l c -> ([mod, [toUpper l]], [c])) ltrs caps
main = writeFile "/home/danl/.XCompose" $ unlines $ ("#include \"%L\"" :) $
  map (\(ks, s) -> concatMap (("<" ++) . (++ ">")) ks ++ ":\"" ++ s ++ "\"") $
  map (first (trig :)) $ (([trig], "'") :) $
  lowCap "1" "aeiouv" "āēīōūǖ" "ĀĒĪŌŪǕ" ++
  lowCap "2" "aceinosuvz" "áćéíńóśúǘź" "ÁĆÉÍÓŃŚÚǗŹ" ++
  lowCap "3" "aeiouv" "ǎěǐǒǔǚ" "ǍĚǏǑǓǙ" ++
  lowCap "4" "aeiouv" "àèìòùǜ" "ÀÈÌÒÙǛ" ++
  lowCap "6" "aceghijosuyz" "âĉêĝĥîĵôŝûŷẑ" "ÂĈÊĜĤÎĴÔŜÛŶẐ"  ++
  lowCap "semicolon" "aeiou" "äëïöü" "ÄËÏÖÜ" ++
  lowCap "9" "u" "ŭ" "Ŭ" ++
  lowCap "o" "a" "å" "Å" ++
  lowCap "c" "c" "ç" "Ç" ++
  lowCap "n" "ano" "ãñõ" "ÃÑÕ" ++
  lowCap "s" "s" "ß" "ẞ" ++
  lowCap "slash" "lo" "łø" "ŁØ" ++
  lowCap "period" "z" "ż" "Ż" ++
  lowCap "comma" "ae" "ąę" "ĄĘ" ++
  [(["h"],"ʻ")] ++
  [(["a","e"],"æ")] ++ [(["A","e"],"Æ")] ++
  [(["o","e"],"œ")] ++ [(["O","e"],"Œ")] ++
  map (\k -> ([kName k], "'" ++ kS k)) (specials ++ map (\c -> Key [c] [c]) (
    ['0'..'9'] ++ ['A'..'Z'] ++ "abdefgijklmopqrstuvwxyz"))
-}
