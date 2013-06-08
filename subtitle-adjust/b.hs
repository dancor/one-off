#include <h>

readSubt :: String -> ((Int, Int), String)
readSubt l = ((read p1, read p2), p3)
  where
    (p1, rest) = break (== '}') $ drop 1 l
    (p2, p3curly) = break (== '}') $ drop 2 rest
    p3 = drop 1 p3curly

showSubt :: ((Int, Int), String) -> String
showSubt ((t1, t2), s) = "{" ++ show t1 ++ "}{" ++ show t2 ++ "}" ++ s

adjust f = first (bothond f)

main :: IO ()
main = do
    ls <- map readSubt . lines <$> getContents
    putStr . unlines $ map (showSubt . adjust (\ x -> x - 336)) ls
