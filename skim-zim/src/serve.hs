{-# LANGUAGE OverloadedStrings #-}
import Codec.Archive.Zim.Parser (getMainPageUrl, getContent, Url(..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Search as BSS
import Data.List
import Data.Maybe
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
main = getArgs >>= \fps -> scotty 3000 (
    get "/" (redirectToZimMainPage fps) >> notFound (serveZimUrl fps))
redirectToZimMainPage :: [FilePath] -> ActionM ()
redirectToZimMainPage fps = 
  liftIO (fmap catMaybes $ mapM getMainPageUrl fps) >>= \res -> case res of
    Url url : _ -> redirect . fromStrict $ decodeUtf8 url
    _ -> status status404 >> text "Could not find ZIM file main page."
keepLangs :: [BSL.ByteString]
keepLangs =
  [ "Arabic"
  , "English"
  , "Danish"
  , "German"
  , "Greek"
  , "Chinese"
  , "French"
  , "Greenlandic"
  , "Hebrew"
  , "Hawaiian"
  -- , "Hindi"
  , "Japanese"
  , "Mandarin"
  , "Middle English"
  , "Middle French"
  , "Norwegian"
  , "Norwegian Bokmål"
  , "Norwegian Nynorsk"
  , "Old English"
  , "Old French"
  , "Proto-Indo-European"  -- maybe just all Proto- ?
  , "Proto-Germanic"
  , "Proto-Sino-Tibetan"
  , "Polish"
  , "Portuguese"
  -- , "Sanskrit"
  , "Spanish"
  , "Swedish"
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
fixColors :: BS.ByteString -> BSLC.ByteString
fixColors = BSS.replace
  ("background-color: white" :: BS.ByteString)
  ("background-color:#777777" :: BS.ByteString)
fixColorsT :: DTL.Text -> DTL.Text
fixColorsT = DTL.replace
  ("background-color: white")
  ("background-color:#777777")
-- Modify the wiktionary article html to only keep the h2 headings that
-- match the languages-of-interest. Also move any English h2 heading from
-- the top to the bottom.
-- 
-- If in the future we actually modify the .zim file we can actually remove
-- articles where no h2 entries remain.
procArticle :: BSL.ByteString -> DTL.Text
procArticle zimHtml = DTLE.decodeUtf8With errLol . TS.renderTags $
  preH2 ++ concat modifiedKeptH2s ++ theEnd where
  (preH2, h2s, theEnd) =
      headerMiddersFooter isH2Id (== TS.TagComment "htdig_noindex")
      (TS.parseTags zimHtml)
  (enH2s, nonEnKeptH2s) = partition ((== "English") . h2IdLang . head) $
      filter ((`elem` keepLangs) . h2IdLang . head) h2s
  (ptH2s, otherKeptH2s) = partition ((== "Portuguese") . h2IdLang . head)
      nonEnKeptH2s
  modifiedKeptH2s = map modifyH2 $ ptH2s ++ otherKeptH2s ++ enH2s
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
        then raw $ fixColors $ BS.concat $ BSL.toChunks zimHtml
        else html $ fixColorsT $ procArticle zimHtml <>
            DTLE.decodeUtf8With errLol (BSL.concat $ map snd rest)
    _ -> do
      liftIO . putStrLn $ "Invalid URL: " ++ show url
      status status404
      text $ "Invalid URL!"
