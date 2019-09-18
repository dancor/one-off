{-# LANGUAGE OverloadedStrings #-}

import Codec.Archive.Zim.Parser (getMainPageUrl, getContent, Url(..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Encoding as DTLE
import Data.Word
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

keepLangs :: [BSL.ByteString]
keepLangs =
    [ "Arabic"
    , "English"
    , "German"
    , "Greek"
    , "Chinese"
    , "French"
    , "Hebrew"
    , "Hawaiian"
    -- , "Hindi"
    , "Japanese"
    , "Mandarin"
    , "Middle English"
    , "Middle French"
    , "Old English"
    , "Old French"
    , "Proto-Indo-European"  -- maybe just all Proto- ?
    , "Proto-Germanic"
    , "Proto-Sino-Tibetan"
    , "Portuguese"
    -- , "Sanskrit"
    , "Spanish"
    , "Translingual"
    ]

isH2Id :: TS.Tag BSL.ByteString -> Bool
isH2Id (TS.TagOpen "h2" (("id", _):_)) = True
isH2Id _ = False

h2IdLang :: TS.Tag BSL.ByteString -> BSL.ByteString
h2IdLang (TS.TagOpen "h2" (("id", lang):_)) = lang
h2IdLang _ = error "h2IdLang x without isH2Id x"

liLang :: [TS.Tag BSLC.ByteString] -> Maybe BSLC.ByteString
liLang = listToMaybe . catMaybes . map liTagLang

liTagLang :: TS.Tag BSLC.ByteString -> Maybe BSLC.ByteString
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

-- Perform a modification to list segments defined by start and end tests.
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

-- Modify the wiktionary article html to only keep the h2 headings that
-- match the languages-of-interest. Also move any English h2 heading from
-- the top to the bottom.
-- 
-- If in the future we actually modify the .zim file we can actually remove
-- articles where no h2 entries remain.
procArticle :: BSL.ByteString -> DTL.Text
procArticle zimHtml =
    DTLE.decodeUtf8With errLol . TS.renderTags $
    preH2 ++ concat modifiedKeptH2s ++ theEnd
  where
    (preH2, h2s, theEnd) =
        headerMiddersFooter isH2Id (== TS.TagComment "htdig_noindex")
        (TS.parseTags zimHtml)
    (engH2s, nonEngKeptH2s) = partition ((== "English") . h2IdLang . head) $
        filter ((`elem` keepLangs) . h2IdLang . head) h2s
    (spaH2s, otherKeptH2s) = partition ((== "Spanish") . h2IdLang . head)
        nonEngKeptH2s
    modifiedKeptH2s = map modifyH2 $ spaH2s ++ otherKeptH2s ++ engH2s
    modifyH2 h2 =
        modifyRegions
            (== TS.TagOpen "h3" [("id","Glyph_origin")])
            (== TS.TagClose "table")
            (\_ -> []) $
        modifyRegions
            (== TS.TagOpen "li" [])
            (== TS.TagClose "li")
            (\li -> if keepLi li then li else [])
        h2
    keepLi = (\x -> isNothing x || fromJust x `elem` keepLangs) . liLang

errLol :: String -> Maybe Word8 -> Maybe Char
errLol _ _ = Nothing

serveZimUrl :: [FilePath] -> ActionM ()
serveZimUrl fps = do
    urlOrig <- (BS.tail . encodeUtf8 . toStrict) <$> param "path"
    let (url, forceNoSkim) = if "raw/" `BS.isPrefixOf` urlOrig
          then (BS.drop 4 urlOrig, True)
          else (urlOrig, False)
    res <- liftIO $ fmap catMaybes $ mapM (`getContent` Url url) fps
    case res of
      (mimeType, zimHtml) : rest -> do
        liftIO . putStrLn $ "Serving: " ++ show url
        setHeader "Content-Type" (fromStrict $ decodeUtf8 mimeType)

        -- These shouldn't be skimmed: -/s/style.css -j/head.js -j/body.js
        if forceNoSkim || "-" `BS.isPrefixOf` url
          then raw zimHtml
          else html $ procArticle zimHtml <>
              DTLE.decodeUtf8With errLol (BSL.concat $ map snd rest)

        {-
        when (url == "A/\232\143\156\229\150\174.html") $ liftIO $ do
            BSLC.writeFile "/home/danl/the-html" zimHtml
            writeFile "/home/danl/the-tags" $
                unlines $ map show $ TS.parseTags zimHtml
            DTLI.writeFile "/home/danl/the-res" $
                DTLE.decodeUtf8With errLol $ 
                --BSLC.filter (`notElem` ("\xa0\x40\x69\x64" :: String)) $
                --BSLC.map (\x -> if x == '\x40' then '@' else x) $
                --BSLC.map (\x -> if x == '\xa0' then '@' else x) $
                TS.renderTags $ TS.parseTags zimHtml
                -}
      _ -> do
        liftIO . putStrLn $ "Invalid URL: " ++ show url
        status status404
        text $ "Invalid URL!"
