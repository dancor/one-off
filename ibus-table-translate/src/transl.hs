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

doRepls l = map doRepl code ++ rest
  where
    (code, rest) = break (== '\t') l

main = do
    ls <- lines <$> getContents
    let (header, table) = break (== "BEGIN_TABLE") ls
        tableInnards = tail $ init table
        semicolonLine = "o\t；\t0"
    putStr . unlines $
        header ++ [head table, semicolonLine] ++
        map doRepls tableInnards ++ [last table]
