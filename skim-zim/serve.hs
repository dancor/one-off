{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Search as BSLS
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Environment (getArgs)
import Network.HTTP.Types.Status (status404)
import Web.Scotty
import Codec.Archive.Zim.Parser (getMainPageUrl, getContent, Url(..))

main :: IO ()
main = do
    [fp] <- getArgs
    scotty 3000 $ do
      get "/" (redirectToZimMainPage fp)
      get (regex "^/(./.*)$") (serveZimUrl fp)
      notFound $ text "Invalid URL!"

redirectToZimMainPage :: FilePath -> ActionM ()
redirectToZimMainPage fp = do
    res <- liftIO $ getMainPageUrl fp
    case res of
      Nothing -> do
        status status404
        text "This ZIM file has no main page specified!"
      Just (Url url) -> redirect . fromStrict $ decodeUtf8 url

keepLangs =
    [ "English"
    , "Spanish"
    , "German"
    ]

serveZimUrl :: FilePath -> ActionM ()
serveZimUrl fp = do
    url <- (encodeUtf8 . toStrict) <$> param "1"
    res <- liftIO $ fp `getContent` Url url
    case res of
      Nothing -> do
        liftIO . putStrLn $ "Invalid URL: " ++ show url
        status status404
        text $ "Invalid URL!"
      Just (mimeType, content) -> do
        liftIO . putStrLn $ "Serving: " ++ show url
        setHeader "Content-Type" (fromStrict $ decodeUtf8 mimeType)
        let (prefooter, footer) = BSLS.breakOn "<!--htdig_noindex-->" content
            h2TagStart = "<h2 id=\""
            (intro:h2s) = BSLS.splitKeepEnd h2TagStart prefooter
            keptH2s = filter (\h2 -> any (`BSL.isPrefixOf` h2) keepLangs) h2s
            keptContent = BSL.concat (intro:keptH2s) <> footer
        -- future note: if null keptH2s actually remove entry from zim file..
        -- but not for: -/s/style.css -j/head.js -j/body.js
        raw keptContent
