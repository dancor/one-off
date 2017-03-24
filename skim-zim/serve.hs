{-# LANGUAGE OverloadedStrings #-}

import Codec.Archive.Zim.Parser (getMainPageUrl, getContent, Url(..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Search as BSLS
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
    , "German"
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

{-
modMids :: (a -> Bool) -> (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
modMids startsMid startsFoot modF xs = concat (h : map modF ms) ++ f
  where
    (h, ms, f) = headMidsFoot startsMid startsFoot xs
-}

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

{-
wtf1 :: BS.ByteString
wtf1 = "�"

wtf2 :: BSL.ByteString
wtf2 = " "
-}

-- future note: if no h2 entries remain, actually remove entry from zim file.
procContent :: BSL.ByteString -> BSL.ByteString
procContent input = TS.renderTags $ preH2 ++ concat modifiedKeptH2s
  where
    (preH2, h2s, _) =
        headerMiddersFooter isH2Id (== TS.TagComment "htdig_noindex")
        (TS.parseTags input)
    keptH2s = filter ((`elem` keepLangs) . h2IdLang . head) h2s
    modifiedKeptH2s = map modifyH2 keptH2s
    modifyH2 h2 = modifyRegions (== TS.TagOpen "li" []) (== TS.TagClose "li")
        (\li -> if keepLi li then li else []) h2
    keepLi = (\x -> isNothing x || fromJust x `elem` keepLangs) . liLang

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
        -- This doesn't process e.g.: -/s/style.css -j/head.js -j/body.js
        raw $ if "-" `BS.isPrefixOf` url then content else procContent content
