import Data.Char
import HSH
import Network.HTTP
import System.Directory
import System.Exit
import System.FilePath

import FlattenTitle

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

myRun :: (String, [String]) -> IO ExitCode
myRun = run

userAgent = "--user-agent=User-Agent: Mozilla/5.0 " ++
    "(Windows NT 10.0; Win64; x64) AppleWebKit/537.36 " ++
    "(KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"

doWget flatTitle = do
    putStrLn $ "Getting: " ++ flatTitle
    cd "m"
    myRun ("wget",
        [ "-nc", "--wait=5", "--random-wait", userAgent
        , "http://www.rottentomatoes.com/m/" ++ flatTitle
        ])
    cd ".."
    cd "tv"
    myRun ("wget",
        [ "-nc", "--wait=5", "--random-wait", userAgent
        , "http://www.rottentomatoes.com/tv/" ++ flatTitle
        ])
    cd ".."

getTitle title = do
    let flatTitle = flattenTitle title
    isCached <- doesFileExist $ "cached" </> flatTitle
    if isCached
      then putStrLn $ "Cached: " ++ flatTitle
      else doWget flatTitle

main :: IO ()
main = do
    titles <- fmap lines $ getContents
    mapM_ getTitle titles
