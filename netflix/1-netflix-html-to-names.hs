import System.Environment
import Text.HTML.TagSoup

main :: IO ()
main = do
    [f] <- getArgs
    tags <- fmap parseTags $ readFile f
    let isAudDes = (`elem` ["Audio Description", "Audio descriptivo"])
        titles = takeWhile (not . isAudDes) $
            map fromTagText $ drop 12 $ filter isTagText tags
    mapM_ putStrLn titles
