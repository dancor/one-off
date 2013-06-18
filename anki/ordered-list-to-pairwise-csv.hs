#include <h>

main = do
    ls <- lines <$> readFile "aaa"
    writeFile "aaa.csv" . unlines $
        zipWith (\ a b -> "\"" ++ a ++ "\",\"" ++ b ++ "\"") ls (tail ls)
