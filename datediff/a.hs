#include <h>

main = do
  r <- (`diffDays` fromGregorian 2011 2 26) . localDay . zonedTimeToLocalTime <$> getZonedTime
  print r
