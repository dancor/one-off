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

sIsLoop s = sFirst s == sLast s

sheepToNet :: Set.Set String -> [(String, String)] -> Map.Map String [String]
sheepToNet loopIdSet tranIds =
    Map.fromListWith (++) $ map (\(x, y) -> (x, [y])) $
    filter (\(x, y) -> x `Set.member` loopIdSet && y `Set.member` loopIdSet)
    tranIds

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

startNet pIn eDir loopMap tranMap net loopRecency = do
    randList <- getStdRandom $ randPerm (Map.toList loopRecency)
    let loopChoice = fst $ maximumBy (bigNoth `on` snd) randList
    spanNet pIn eDir loopMap tranMap net loopChoice loopRecency

exhaust h = do
    ready <- hReady h
    when ready $ do
      l <- hGetLine h
      --putStrLn l
      exhaust h

waitForDemuxEof h = do
    l <- hGetLine h
    --putStrLn l
    --unless (l == "  DEMUXER: ds_fill_buffer: EOF reached (stream: video)\n") $
    unless ("EOF" `isInfixOf` l) $
        waitForDemuxEof h

spanNet pIn@(pInn, pOut, pErr, pId) eDir loopMap tranMap net loopChoice loopRecency = do
    let Just loopFilename = Map.lookup loopChoice loopMap
        reper 0 = return ()
        reper n = do
            hPutStrLn pInn $ "loadfile " ++ eDir </> loopFilename
            --putStrLn $ "loadfile " ++ eDir </> loopFilename
            hFlush pInn
            waitForDemuxEof pOut
            exhaust pErr
            reper (n - 1)
    reper 6
    case Map.lookup loopChoice net of
      Nothing -> do
          hPutStr stderr "Unexpected sheep dead-end, restarting sheep net."
          startNet pIn eDir loopMap tranMap net loopRecency
      Just tranList -> do
          let tranRecencies = zip tranList $
                  catMaybes [Map.lookup tran loopRecency | tran <- tranList]
          randList <- getStdRandom $ randPerm tranRecencies
          let tranChoice = fst $ maximumBy (bigNoth `on` snd) randList
              Just tranFilename = Map.lookup (loopChoice, tranChoice) tranMap
          hPutStrLn pInn $ "loadfile " ++ eDir </> tranFilename
          --putStrLn $ "loadfile " ++ eDir </> tranFilename
          hFlush pInn
          waitForDemuxEof pOut
          exhaust pErr
          let myUpdater Nothing = Just 1
              myUpdater (Just x) = Just (x + 1)
          spanNet pIn eDir loopMap tranMap net tranChoice $
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

        -- We only consider loops that aren't dead-ends.
        loopIdSet = Set.fromList (Map.keys loopMap)
        -- Actually, seems like there are fewer complex loops (such
        -- as 2-cycles) if we leave the dead-ends and just have those
        -- occasional jumps.
            -- `Set.intersection` Set.fromList (map fst $ Map.keys tranMap)

        net = sheepToNet loopIdSet (Map.keys tranMap)
        loopRecency = Map.fromList $
            zip (Set.toList loopIdSet) (repeat Nothing)
    --mapM_ print $ Map.toList net
    putStrLn $ show (Set.size loopIdSet) ++ " sheep loops."
    let opts = words "-fs -fixed-vo -really-quiet -slave -idle -nolirc -msgmodule -msglevel demuxer=9:statusline=0"
    (pIn, pOut, pErr, pId) <- runInteractiveProcess "/usr/bin/mplayer" opts Nothing Nothing
    startNet (pIn, pOut, pErr, pId) eDir loopMap tranMap net loopRecency
