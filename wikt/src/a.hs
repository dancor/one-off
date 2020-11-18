-- Usage: bzless ~/data/wikt/en.xml.bz2 | rhs a.hs > out.txt
#include <h>
import Data.Tree

type Entry = Forest T
type Ts = [T]
type T = TL.Text

page :: (a -> Bool) -> [a] -> [[a]]
page _       []     = []
page isStart (x:xs) = if isStart x
  then pageA isStart [x] xs else page isStart xs

pageA :: (a -> Bool) -> [a] -> [a] -> [[a]]
pageA _       ys []     = [ys]
pageA isStart ys (x:xs) = if isStart x
  then ys : pageA isStart [x] xs else pageA isStart (ys ++ [x]) xs

titlePre, titlePost, shaPre :: T
titlePre = "    <title>"
titlePost = "</title>"
shaPre = "      <sha1>"
textPost = "</text>"

takeMid :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeMid l r = takeWhile (not . r) . drop 1 . dropWhile (not . l)

procTitleLine :: T -> T
procTitleLine titleLine = if post == titlePost then title
    else error $ "procTitleLine: " ++ show titleLine
  where
    (title, post) = TL.splitAt (TL.length t - TL.length titlePost) t
    t = TL.drop (TL.length titlePre) titleLine

onLast :: (a -> a) -> [a] -> [a]
onLast f [x] = [f x]
onLast f (x:xs) = x : onLast f xs
onLast _ [] = []

onHead :: (a -> a) -> [a] -> [a]
onHead f (x:xs) = f x : xs
onHead _ [] = []

tlOnHead :: (Char -> Char) -> TL.Text -> TL.Text
tlOnHead f t = case TL.uncons t of
  Just (c, t2) -> TL.cons (f c) t2
  _ -> t

killText :: T -> T
killText l = if textPost `TL.isSuffixOf` l
  then TL.dropEnd (TL.length textPost) l else l

procPage :: (T, Ts) -> Maybe (T, Entry)
procPage (titleLine, ls) = case takeMid (== "==Portuguese==")
    (\l -> "==" `TL.isPrefixOf` l && not ("===" `TL.isPrefixOf` l)) ls of
  [] -> Nothing
  m -> Just (procTitleLine titleLine, 
    readEntry . onLast killText . filter (/= "----") $ filter (/= "") m)

procLineType :: T -> (Int, T)
procLineType l = if TL.all (== '=') post
    then (fromIntegral depth, TL.concat . map (tlOnHead toUpper) $ TL.words mid)
    else error $ "procLineType: " ++ show l
  where
    (depth, midPost) = first TL.length $ TL.span (== '=') l
    (mid, post) = TL.splitAt (TL.length midPost - depth) midPost

readEntry :: Ts -> Entry
readEntry l = gs . go0 $ map procLineType l
  where
    go0 :: [(Int,T)] -> Entry
    go0 ((0,_):xs) = go0 xs  -- (0,"{{cardinalbox|pt||0|1||um}}") for "zero"
    -- Usually starts at depth 3, but some mistakenly start at 4.
    go0 ((depth,heading):rest) = r where (r,[]) = go depth heading [] rest
    go0 [] = []  -- error $ "readEntry" ++ show l
    -- go: get anything at or greater than current depth
    go :: Int -> T -> Forest T -> [(Int,T)] -> (Forest T, [(Int,T)])
    go depth h ys [] = ([Node h ys], [])
    go depth h ys ((0,x):xs) = go depth h (ys ++ [Node x []]) xs
    go depth h ys ((depth2,x):xs) = case compare depth depth2 of
      LT -> let (ys2, rest) = go depth2 x [] xs in go depth h (ys ++ ys2) rest
      EQ -> let (ys2, rest) = go depth x [] xs in (Node h ys : ys2, rest)
      GT -> ([Node h ys], [])
    gs = concatMap g
    g :: Tree T -> [Tree T]
    g x@(Node l kids) = if "{{head|pt|" `TL.isPrefixOf` l || l `elem`
      ["AlternativeForms","DerivedTerms","Descendants","Pronunciation",
      "Quotations","RelatedTerm","Synonyms"]
      then []
      else if "Etymology" `TL.isPrefixOf` l
        then gs $ filter (not . null . subForest) kids
        else [Node l (gs kids)]

main = do
    pages <- catMaybes . map (procPage . fromJust . uncons) .
        page (\l -> titlePre `TL.isPrefixOf` l || shaPre `TL.isPrefixOf` l) .
        TL.lines <$> TL.getContents
    mapM_ (\(t,f) -> TL.putStrLn . TL.replace "{{l|en|" "" .
        TL.replace "{{pt-" "" . TL.replace "}}" "" . 
        TL.intercalate " " $ filter (not . TL.null) 
        (t:concatMap flatten f)) pages
