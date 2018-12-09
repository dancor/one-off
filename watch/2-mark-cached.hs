import Share

import qualified Data.HashSet as HS

main :: IO ()
main = do
    nums <- getAllNetflixNums
    writeFile "scrape/rotten-tomatoes/cache" $ unlines $ HS.toList nums
