import Control.Applicative
import Data.Char
import System.Environment
import System.FilePath

import FlattenTitle

main :: IO ()
main = do
    args <- getArgs
    titles <- map flattenTitle . concatMap lines <$> mapM readFile args
    mapM_ (\(Title t) -> writeFile ("cached" </> t) "") titles
