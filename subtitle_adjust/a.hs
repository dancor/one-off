#include <h>

addNum :: Int
addNum = 86500 + 12

myF :: String -> String
myF l = 
    "{" ++
    show (read b + addNum) ++ "}{" ++
    show (read d + addNum) ++ "}" ++ e
  where
    [a, b, c, d, e] = splitWhen (`elem` "{}") l

main = do
  ls <- map myF . lines <$> readFile "subs.txt"
  writeFile "subsMod.txt" $ unlines ls
