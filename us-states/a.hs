#include <h>

main = interactL $ \ ls ->
  map (\ a -> show (length a) ++ " " ++ show a) $ 
  groupBy ((==) `on` (\ (_:a:_) -> a)) $ sort ls
  