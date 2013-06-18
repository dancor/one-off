import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Parallel.Strategies
import Criterion.Main
import Data.Aeson
import Data.Attoparsec
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified Data.HashMap.Strict as HMS
import Data.List.Split
import Data.Maybe
import Prelude hiding (catch)
import System.IO.Error hiding (catch)

readLines :: IO [BS.ByteString]
readLines =
    liftM2 (:) BS.getLine readLines
    `catch`
    (\e -> if isEOFError e then return [] else ioError e)

parseLine :: BS.ByteString -> HMS.HashMap DT.Text Value
parseLine l = result
  where
    Done _ (Object result) = parse json l

valToStr :: Value -> DT.Text
valToStr (String x) = x
valToStr _ = error "valToStr"

showEntry :: HMS.HashMap DT.Text Value -> DT.Text
showEntry e = DT.unwords [lkup $ DT.pack "a", lkup $ DT.pack "missing"]
  where
    lkup k = fromMaybe (DT.pack "???") $ valToStr <$> HMS.lookup k e

main :: IO ()
main = do
    ls <- map parseLine <$> readLines
    -- let res = parMap rdeepseq (map showEntry) (chunksOf 100 ls)
    let res = parMap rdeepseq showEntry ls
    mapM_ DTI.putStrLn res
