import Data.List
import System.Environment
import Text.HTML.TagSoup

-- You can find this for additional languages by looking a few Words after
-- second instance of member-footer-link-content in the .html file.
isAudDes :: String -> Bool
isAudDes x = x `elem`
  [ "Audio Description" -- en
  , "Audio descriptivo" -- es
  , "Audiodeskription"  -- de
  , "口述影像"          -- zh
  ]

doFile :: FilePath -> IO ()
doFile f = do
    let newF = if ".html" `isSuffixOf` f
          then take (length f - 5) f ++ ".txt"
          else error "Expecting filename to end in .html"
    putStrLn $ "Creating " ++ newF
    tags <- fmap parseTags $ readFile f
    let titles = takeWhile (not . isAudDes) $
            map fromTagText $ drop 12 $ filter isTagText tags
    writeFile newF $ unlines titles

main :: IO ()
main = do
    args <- getArgs
    mapM_ doFile args
