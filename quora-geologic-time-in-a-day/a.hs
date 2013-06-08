midnight = 4600 * 1000 * 1000

secPerDay = 24 * 60 * 60

times =
    map (* 1000) [3000, 2000, 1800, 1750, 700, 500, 195, 160, 150, 130,
                  100] ++
    [82500, 74000, 40000, 32000, 30000, 27000, 20000, 15000, 10000,
     8000, 6500, 3000, 1000, 150, 40]

secPerTime t = fromIntegral (t * secPerDay) / fromIntegral midnight

main = do
    putStr . unlines $
        map (\ t -> 
            "11:59:" ++ show (60 - secPerTime t) ++ " pm (" ++ show t ++ " years ago)")
        times
