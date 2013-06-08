#include <h>

isMusFile :: String -> Bool
isMusFile f = "ogg" `isSuffixOf` f || "mp3" `isSuffixOf` f

data MusFileParts
    = MusFileParts
    { mNum :: String
    , mName :: String
    , mSuf :: String
    , mFull :: String
    }
    deriving (Show)

musFileParts :: String -> MusFileParts
musFileParts f = MusFileParts num (init nameDot) suf f
  where
    (num, ' ':rest) = break (== ' ') f
    (nameDot, suf) = reversifyTup (break (== '.')) rest

runP x@(x0:xs) = do
    print x
    createProcess $ proc x0 xs

main :: IO ()
main = do
    fs <- map musFileParts . filter isMusFile <$> getDirectoryContents "."
    forM_ fs $ \ f -> do
        runP ["id3v2", "-s", mFull f]
        runP ["id3v2", "-2T", mNum f, mFull f]
        runP ["id3v2", "-2t", mName f, mFull f]
    putStr $ unlines $ map show fs
