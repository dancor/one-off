#include <h>

startDay = fromGregorian 2018 6 7

numDays = 900

showDay = formatTime defaultTimeLocale "%F %a"

main =
    mapM_ (\n -> putStrLn $ showDay $ addDays n startDay) $ take numDays [0..]
