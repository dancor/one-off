import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Random

data Sheep = Sheep
    { sFirst :: String
    , sLast :: String
    , sFilename :: String
    }

parseFilename :: String -> Sheep
parseFilename filename = Sheep theFirst theLast filename
    where
    goodStart = drop 12 filename
    (theFirst, rest) = splitAt 5 goodStart
    theLast = take 5 $ drop 1 rest

sIsLoop :: Sheep -> Bool
sIsLoop s = sFirst s == sLast s

sheepToNet :: Set.Set String -> [(String, String)] -> Map.Map String [String]
sheepToNet loopIdSet tranIds =
    Map.fromListWith (++) $ map (\(x, y) -> (x, [y])) $
    filter (\(x, y) -> x `Set.member` loopIdSet && y `Set.member` loopIdSet)
    tranIds

bigNoth :: (Ord a) => Maybe a -> Maybe a -> Ordering
bigNoth Nothing Nothing = EQ
bigNoth Nothing _ = GT
bigNoth _ Nothing = LT
bigNoth (Just x) (Just y) = compare x y

randPerm :: [a] -> StdGen -> ([a], StdGen)
randPerm [] g = ([], g)
randPerm xs g = (xs !! n : perm, g3)
  where
    (n, g2) = randomR (0, length xs - 1) g
    (perm, g3) = randPerm (take n xs ++ drop (n + 1) xs) g2

startNet
    :: (Handle, Handle, Handle)
    -> FilePath
    -> Map.Map String FilePath
    -> Map.Map (String, String) FilePath
    -> Map.Map String [String]
    -> Map.Map String (Maybe Int)
    -> IO a
startNet gfxP eDir loopMap tranMap net loopRecency = do
    randList <- getStdRandom $ randPerm (Map.toList loopRecency)
    let loopChoice = fst $ maximumBy (bigNoth `on` snd) randList
    spanNet gfxP eDir loopMap tranMap net loopChoice loopRecency

exhaust :: Handle -> IO ()
exhaust h = do
    ready <- hReady h
    when ready $ do
        _ <- hGetLine h
        exhaust h

waitForDemuxEof :: Handle -> IO ()
waitForDemuxEof h = do
    l <- hGetLine h
    unless ("EOF" `isInfixOf` l) $
        waitForDemuxEof h

spanNet
    :: (Handle, Handle, Handle)
    -> FilePath
    -> Map.Map String FilePath
    -> Map.Map (String, String) FilePath
    -> Map.Map String [String]
    -> String
    -> Map.Map String (Maybe Int)
    -> IO a
spanNet gfxP@(pIn, pOut, pErr) eDir loopMap tranMap net loopChoice
        loopRecency = do
    let Just loopFilename = Map.lookup loopChoice loopMap
    replicateM_ 6 $ do
        hPutStrLn pIn $ "loadfile " ++ eDir </> loopFilename
        hFlush pIn
        waitForDemuxEof pOut
        exhaust pErr
    case Map.lookup loopChoice net of
      Nothing -> do
          hPutStr stderr "Unexpected sheep dead-end, restarting sheep net."
          startNet gfxP eDir loopMap tranMap net loopRecency
      Just tranList -> do
          let tranRecencies = zip tranList $
                  catMaybes [Map.lookup tran loopRecency | tran <- tranList]
          randList <- getStdRandom $ randPerm tranRecencies
          let tranChoice = fst $ maximumBy (bigNoth `on` snd) randList
              Just tranFilename = Map.lookup (loopChoice, tranChoice) tranMap
          hPutStrLn pIn $ "loadfile " ++ eDir </> tranFilename
          hFlush pIn
          waitForDemuxEof pOut
          exhaust pErr
          let myUpdater Nothing = Just 1
              myUpdater (Just x) = Just (x + 1)
          spanNet gfxP eDir loopMap tranMap net tranChoice $
              Map.adjust myUpdater loopChoice loopRecency

main :: IO ()
main = do
    home <- getHomeDirectory
    let eDir = home </> ".electricsheep"
    (loops, trans) <- partition sIsLoop . map parseFilename .
        filter (".avi" `isSuffixOf`) <$> getDirectoryContents eDir
    let loopMap = Map.fromList $ map (\s -> (sFirst s, sFilename s)) loops
        tranMap = Map.fromList $
            map (\s -> ((sFirst s, sLast s), sFilename s)) trans
        loopIdSet = Set.fromList (Map.keys loopMap)
        net = sheepToNet loopIdSet (Map.keys tranMap)
        loopRecency = Map.fromList $
            zip (Set.toList loopIdSet) (repeat Nothing)
    putStrLn $ show (Set.size loopIdSet) ++ " sheep loops."
    let opts =
            [ "-fs", "-fixed-vo", "-really-quiet", "-slave", "-idle"
            , "-nolirc", "-msgmodule", "-msglevel", "demuxer=9:statusline=0"]
    (pIn, pOut, pErr, _) <-
        runInteractiveProcess "/usr/bin/mplayer" opts Nothing Nothing
    startNet (pIn, pOut, pErr) eDir loopMap tranMap net loopRecency
