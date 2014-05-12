#include <h>

import qualified Codec.Compression.GZip as GZip
import Data.Hash.MD5
import Network.HTTP

data Sheep a = Sheep
    { sId :: a
    , sFirst :: a
    , sLast :: a
    , sUrl :: a
    , sName :: a
    } deriving Show

procKid gen elem = Sheep kId kFirst kLast kUrl kName
  where
    Just kId = TXL.findAttr (TXL.unqual "id") elem
    Just kFirst = TXL.findAttr (TXL.unqual "first") elem
    Just kLast = TXL.findAttr (TXL.unqual "last") elem
    Just kUrl = TXL.findAttr (TXL.unqual "url") elem
    kName = 
        gen ++ "=" ++ kId ++ "=" ++ kFirst ++ "=" ++ kLast ++ ".avi"

genFromRaw genRaw =
    replicate (5 - length iStr) '0' ++ iStr
  where
    i = read genRaw
    iStr = show (i :: Int)

main :: IO ()
main = do
    let host = "http://v2d7c.sheepserver.net"
        uid = "dancor"
        version = "LNX_2.7b12"
        url = host </> "cgi/list?u=" ++ uid ++ "&v=" ++ version
    resp <- simpleHTTP $ getRequest url
    body <- getResponseBody resp
    let theElem =
            head .
            TXL.onlyElems $ 
            TXL.parseXML $ GZip.decompress $ BSLC.pack body
        Just genRaw = TXL.findAttr (TXL.unqual "gen") theElem
        gen = genFromRaw genRaw
        kids = map (procKid gen) $
            TXL.findChildren (TXL.unqual "sheep") theElem
    home <- getHomeDirectory
    let eDir = home </> ".electricsheep"
    forM_ kids $ \k -> do
        alreadyGot <- doesFileExist $ eDir </> sName k
        unless alreadyGot $ do
            putStrLn $ "Getting: " ++ sName k
            rawSystem "curl" [sUrl k, "-o", eDir </> "sheep.avi"]
            rawSystem "mv" [eDir </> "sheep.avi", eDir </> sName k]
            putStrLn "Done."
