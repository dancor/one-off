{-# LANGUAGE OverloadedStrings #-}

import Share

#include <h>

main :: IO ()
main = do
    list <- loadList
    mapM_ print $ sortBy (compare `on` snd) $ Map.toList $
        Map.fromListWith (+) $ map (flip (,) (1 :: Int)) $
        -- concatMap (fst . snd) list
        map (head . fst . snd) list
