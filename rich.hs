import System.Environment

cur = 1692540.85

pay1 = 82500
day1 = 14

pay2 = 82500
day2 = 14 + 122

pay3 = 1520000
day3 = 14 + 122 + 62

--rate = 1 + 1.34 / 100
--rate = 1 + 1.98 / 100

main = do
  [rateS] <- getArgs
  let
    rate = read rateS
    curVal futVal daysFromNow = futVal / (rate ** (daysFromNow / 365))
    x = cur - curVal pay1 day1 - curVal pay2 day2 - curVal pay3 day3
  print $ "Cur val: " ++ show x

