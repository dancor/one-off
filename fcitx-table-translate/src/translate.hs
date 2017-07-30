#include <h>

-- Replace the second column with your keymap.
-- This is Colmak.

doRepl :: Char -> Char

doRepl 'q' = 'q'
doRepl 'w' = 'w'
doRepl 'e' = 'f'
doRepl 'r' = 'p'
doRepl 't' = 'g'
doRepl 'y' = 'j'
doRepl 'u' = 'l'
doRepl 'i' = 'u'
doRepl 'o' = 'y'
doRepl 'p' = ';'

doRepl 'a' = 'a'
doRepl 's' = 'r'
doRepl 'd' = 's'
doRepl 'f' = 't'
doRepl 'g' = 'd'
doRepl 'h' = 'h'
doRepl 'j' = 'n'
doRepl 'k' = 'e'
doRepl 'l' = 'i'
doRepl ';' = 'o'

doRepl 'z' = 'z'
doRepl 'x' = 'x'
doRepl 'c' = 'c'
doRepl 'v' = 'v'
doRepl 'b' = 'b'
doRepl 'n' = 'k'
doRepl 'm' = 'm'

doRepl x = error $ show x

doRepls :: String -> String
doRepls l = map doRepl code ++ rest
  where
    (code, rest) = break (== ' ') l

headerMod :: String -> String
headerMod l
    | l == "KeyCode=abcdefghijklmnopqrstuvwxy" =
           "KeyCode=abcdefghijklmnpqrstuvwxy;"
    | otherwise = l

main :: IO ()
main = do
    ls <- lines <$> getContents
    let (header, table) = break (== "a 工") ls
    putStr . unlines $ map headerMod header ++ map doRepls table
