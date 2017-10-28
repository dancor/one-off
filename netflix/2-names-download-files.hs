import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import HSH
import Network.HTTP
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

import Title

myRun :: (String, [String]) -> IO ExitCode
myRun = run

userAgent :: String
userAgent = "--user-agent=User-Agent: Mozilla/5.0 " ++
    "(Windows NT 10.0; Win64; x64) AppleWebKit/537.36 " ++
    "(KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"

isCached :: Title -> IO Bool
isCached (Title t) = doesFileExist $ "cached" </> t

getTitleEvenIfCached :: Int -> Int -> Title -> IO ()
getTitleEvenIfCached tot cur (Title t) = do
    putStrLn $ "Getting (" ++ show cur ++ "/" ++ show tot ++ "): " ++ t
    cd "m"
    myRun ("wget",
        [ "-nc", "--wait=3", "--random-wait", userAgent
        , "http://www.rottentomatoes.com/m/" ++ t
        ])
    cd ".."
    cd "tv"
    myRun ("wget",
        [ "-nc", "--wait=3", "--random-wait", userAgent
        , "http://www.rottentomatoes.com/tv/" ++ t
        ])
    cd ".."

main :: IO ()
main = do
    args <- getArgs
    titles1 <- map flattenTitle . concatMap lines <$> mapM readFile args
    print $ show (length titles1) ++ " titles before dedupe."
    let titles2 = nub titles1
    print $ show (length titles2) ++ " titles after dedupe."
    titles3 <- filterM (fmap not . isCached) titles2
    let t3l = length titles3
    print $ show t3l ++ " titles after cache check."
    zipWithM_ (getTitleEvenIfCached t3l) [1..] titles3
