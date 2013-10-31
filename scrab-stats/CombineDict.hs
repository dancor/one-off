import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as Set

procLine s l =
    if wd `Set.member` s
      then BSC.cons ' ' l
      else BSC.cons '*' l
  where
    (wd, _) = BSC.break (== ' ') l

main :: IO ()
main = do
    lol <- head . BSC.lines <$> BS.readFile "TWL06.txt"
    print lol

    s <- Set.fromList . BSC.lines <$> BS.readFile "TWL06.txt"
    print $ BSC.pack "QAT" `Set.member` s
    out <- BSC.unlines . map (procLine s) . BSC.lines <$>
        BS.readFile "CSW12.txt"
    BS.writeFile "CSW12TWL06.txt" out
    
