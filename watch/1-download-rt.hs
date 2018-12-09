import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import qualified Data.HashSet as HS
import qualified Data.Text as T
import HSH
import Network.HTTP
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

import Share

myRun :: (String, [String]) -> IO ExitCode
myRun = run

isCached :: Title -> IO Bool
isCached (Title t) = doesFileExist $ "cached" </> t

getTitleEvenIfCached :: Int -> Int -> Title -> IO ()
getTitleEvenIfCached tot cur (Title t) = do
    putStrLn $ "Getting (" ++ show cur ++ "/" ++ show tot ++ "): " ++ t
    cd "standalone"
    myRun ("my-wget", ["https://www.rottentomatoes.com/m/" ++ t])
    cd ".."
    cd "series"
    myRun ("my-wget", ["-O", t,
        "https://www.rottentomatoes.com/tv/" ++ t ++ "/s01"])
    cd ".."

main :: IO ()
main = do
    cache <- HS.fromList . lines <$> readFile "scrape/rotten-tomatoes/cache"
    nums <- filter (not . (`HS.member` cache)) . HS.toList <$>
        getAllNetflixNums
    titles1 <- mapM (fmap (flattenTitle . T.unpack . dTitle . snd) .
        readDescFile) nums
    putStrLn $ show (length titles1) ++ " titles before dedupe."
    let titles2 = nub titles1
        t2l = length titles2
    putStrLn $ show t2l ++ " titles after dedupe."
    cd "scrape/rotten-tomatoes"
    zipWithM_ (getTitleEvenIfCached t2l) [1..] titles2
