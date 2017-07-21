{-# LANGUAGE OverloadedStrings #-}

import Codec.Archive.Zim.Parser (getMainPageUrl, getContent, Url(..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.Search as BSLS
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict, fromStrict)
import Network.HTTP.Types.Status (status404)
import System.Environment (getArgs)
import qualified Text.HTML.TagSoup as TS
import Web.Scotty

main :: IO ()
main = do
    fps <- getArgs
    scotty 3000 $ do
      get "/" (redirectToZimMainPage fps)
      notFound (serveZimUrl fps)

redirectToZimMainPage :: [FilePath] -> ActionM ()
redirectToZimMainPage fps = do
    res <- liftIO $ fmap catMaybes $ mapM getMainPageUrl fps
    case res of
      Url url : _ -> redirect . fromStrict $ decodeUtf8 url
      _ -> status status404 >> text "Could not find ZIM file main page."

keepLangs =
    [ "English"
    , "German"
    , "Greek"
    , "Mandarin"
    , "Middle English"
    , "Old English"
    , "Proto-Indo-European"  -- maybe just all Proto- ?
    , "Proto-Germanic"
    , "Proto-Sino-Tibetan"
    , "Spanish"
    , "Translingual"
    ]

isH2Id (TS.TagOpen "h2" (("id", _):_)) = True
isH2Id _ = False

isTranslationsCell (TS.TagOpen "table" (("class","translations"):_)) = True
isTranslationsCell _ = False

h2IdLang (TS.TagOpen "h2" (("id", lang):_)) = lang
h2IdLang _ = error "h2IdLang x without isH2Id x"

liLang = listToMaybe . catMaybes . map liTagLang

liTagLang (TS.TagText t) =
    if " " `BSL.isPrefixOf` t && ": " `BSL.isSuffixOf` t
      then Just . BSL.tail $ BSL.take (BSL.length t - 2) t
      else Nothing
liTagLang _ = Nothing

headerMiddersFooter :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [[a]], [a])
headerMiddersFooter startsAMidder startsFooter xs =
    (h, TS.partitions startsAMidder m, f)
  where
    (h, rest) = break startsAMidder xs
    (m, f) = break startsFooter rest

modifyRegions :: (a -> Bool) -> (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
modifyRegions startsARegion endsARegion f xs = pre ++
    if null endAndRest
      then regionAndRest
      else f region ++ modifyRegions startsARegion endsARegion f rest
  where
    (pre, regionAndRest) = break startsARegion xs
    (regionMinusEnd, endAndRest) = break endsARegion regionAndRest
    end:rest = endAndRest
    region = regionMinusEnd ++ [end]

-- Bare \160 appear after running eswikt zim html through tagsoup for
-- nonbreaking space characters. But that's not the right utf8. So we fix it.
--
-- I don't know if the error is in tagsoup, zim-parser, or the zim file.
fixNbsp :: BSL.ByteString -> BSL.ByteString
fixNbsp = BSLS.replace nbspFrom nbspTo
  where
    nbspFrom = BSC.pack "\160"
    nbspTo = BSC.pack "\194\160"

-- Modify the wiktionary article html to only keep the h2 headings that
-- match the languages-of-interest. Also move any English h2 heading from
-- the top to the bottom.
-- 
-- If in the future we actually modify the .zim file we can actually remove
-- articles where no h2 entries remain.
procArticle :: BSL.ByteString -> BSL.ByteString
procArticle html =
    fixNbsp . TS.renderTags $ preH2 ++ concat modifiedKeptH2s ++ theEnd
  where
    (preH2, h2s, theEnd) =
        headerMiddersFooter isH2Id (== TS.TagComment "htdig_noindex")
        (TS.parseTags html)
    (engH2s, nonEngKeptH2s) = partition ((== "English") . h2IdLang . head) $
        filter ((`elem` keepLangs) . h2IdLang . head) h2s
    (spaH2s, otherKeptH2s) = partition ((== "Spanish") . h2IdLang . head)
        nonEngKeptH2s
    modifiedKeptH2s = map modifyH2 $ spaH2s ++ otherKeptH2s ++ engH2s
    modifyH2 h2 = modifyRegions (== TS.TagOpen "li" []) (== TS.TagClose "li")
        (\li -> if keepLi li then li else []) h2
    keepLi = (\x -> isNothing x || fromJust x `elem` keepLangs) . liLang

killToHtml :: BSL.ByteString -> BSL.ByteString
killToHtml = id
-- killToHtml = BSLC.dropWhile (/= '>') . snd . BSLS.breakAfter "<body"

serveZimUrl :: [FilePath] -> ActionM ()
serveZimUrl fps = do
    url <- (BS.tail . encodeUtf8 . toStrict) <$> param "path"
    res <- liftIO $ fmap catMaybes $ mapM (`getContent` Url url) fps
    case res of
      (mimeType, html) : rest -> do
        liftIO . putStrLn $ "Serving: " ++ show url
        setHeader "Content-Type" (fromStrict $ decodeUtf8 mimeType)
        -- This doesn't process e.g.: -/s/style.css -j/head.js -j/body.js
        raw $ if "-" `BS.isPrefixOf` url then html else
          procArticle html <> BSL.concat (map (killToHtml . snd) rest)
      _ -> do
        liftIO . putStrLn $ "Invalid URL: " ++ show url
        status status404
        text $ "Invalid URL!"
