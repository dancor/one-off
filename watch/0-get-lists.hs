#include <h>

import Share

getNum num = do
    let f = "scrape/netflix/descs" </> num <> ".html"
    doesExist <- doesFileExist f
    unless doesExist $ do
        putStrLn f
        hshRun ("my-wget", ["-O", f, "--retry-on-http-error=404",
            "https://www.netflix.com/title" </> num])
        Conc.threadDelay 2000000

main = do
    nums <- getAllNetflixNums
    print $ length nums
    mapM_ getNum nums
