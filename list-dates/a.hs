#include <h>

-- right now must be a ????-03-01 day because of how the numbering is done
startDay y = fromGregorian y 3 1

lpad c n s = replicate (n - length s) c <> s

showDay n = formatTime defaultTimeLocale ("%F " <> lpad '0' 3 (show n) <> " %a")

main = do
  args <- getArgs
  case map readMay args of
    [Just year] -> mapM_
      (\n -> putStrLn $ showDay n $ addDays (n - 1) (startDay year)) [1..366]
    _ -> hPutStrLn stderr "Usage: rhs a.hs 2022 > out"
