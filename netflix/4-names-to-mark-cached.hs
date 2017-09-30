import Data.Char
import System.FilePath

import FlattenTitle

main :: IO ()
main = do
    titles <- fmap (map flattenTitle . lines) $ getContents
    mapM_ (\title -> writeFile ("cached" </> title) "") titles
