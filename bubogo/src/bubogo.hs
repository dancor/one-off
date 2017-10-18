-- The higher the digit the better for the player about to play.
-- 9 means 90+% estimated chance of winning.

-- Engine bugs in AQ that we work around (I am using AQ v2.0.2),
-- but our workarounds would still work with correct engines:
-- - genmove [color]: it always ignores color and plays who it thinks is next
-- - play [color] [square]: if color is white it ignores color and plays
--   who is next
-- - use "play pass" to get around these issues

{-# LANGUAGE ParallelListComp #-}
#include <h>

data Engine = Engine
  { eInH  :: !Handle
  , eOutH :: !Handle
  , eErrH :: !Handle
  }

data Color = Black | White deriving (Eq, Show)

data Move = Move
  { mColor  :: !Color
  , mColumn :: !Int
  , mRow    :: !Int
  } deriving Show

type Board = MVec.IOVector (Maybe Color)

getEval :: Handle -> IO String
getEval engErr = do
    l <- hGetLine engErr
    if "1: " `isPrefixOf` l
      then return $ takeWhile (/= '[') (words l !! 3)
      else do
        -- putStrLn l
        getEval engErr

colorLtr :: Color -> String
colorLtr Black = "b"
colorLtr White = "w"

columnStr, rowStr :: Int -> String
columnStr col = [chr (col + ord 'A' + if col >= 8 then 1 else 0)]
rowStr row = show (19 - row)

switchColor :: Color -> Color
switchColor Black = White
switchColor White = Black

decodeColor :: Int -> Either Int Color
decodeColor 10000 = Right Black
decodeColor 20000 = Right White
decodeColor x = Left x

encodeColor :: Either Int Color -> Int
encodeColor (Right Black) = 10000
encodeColor (Right White) = 20000
encodeColor (Left x) = x

ePut :: Engine -> String -> IO ()
ePut e = hPutStrLn (eInH e)

ePlayMove :: Engine -> Move -> IO ()
ePlayMove e (Move color column row) = do
    when (color == White) $ ePut e "play pass"
    ePut e $
        "play " ++ colorLtr color ++ " " ++ columnStr column ++ rowStr row
    when (color == Black) $ ePut e "play pass"

eSetBoard :: Engine -> Board -> IO ()
eSetBoard e b = do
    ePut e "clear_board"
    forM_ [0..18] $ \row -> forM_ [0..18] $ \column -> do
        sqHas <- bRead b column row
        case sqHas of
          Just color -> ePlayMove e (Move color column row)
          _ -> return ()

eSetMoves :: Engine -> [Move] -> IO ()
eSetMoves e moves = ePut e "clear_board" >> mapM_ (ePlayMove e) moves

eGenMove :: Engine -> Color -> IO ()
eGenMove e color = do
    when (color == White) $ ePut e "play pass"
    ePut e $ "genmove " ++ colorLtr color
    when (color == Black) $ ePut e "play pass"
    hFlush (eInH e)

evalAllMoves :: Engine -> Board -> Color -> IO ()
evalAllMoves e b color = do
    let doSq column row = do
            eSetBoard e b
            ePlayMove e (Move color column row)
            eGenMove e (switchColor color)
            eval <- getEval $ eErrH e
            putStr $ show $ 9 - min 9 (floor $ read eval / 10)
    forM_ [0..18] $ \row -> do
        forM_ [0..18] $ \column -> do
            sqHas <- bRead b column row
            case sqHas of
              Just Black -> putStr "●"
              Just White -> putStr "℗"
              _ -> doSq column row
        putStrLn ""

readBoard :: Board -> [String] -> IO ()
readBoard b = zipWithM_ readRow [0..18] . take 19 . tail
  where
    readRow row =
        zipWithM_ (readSq row) [0..18] . take 19 . everyOther . drop 2
    readSq row column = bWrite b column row . decode
    everyOther (x:_:xs) = x : everyOther xs
    everyOther _ = []
    decode '●' = Just Black
    decode '℗' = Just White
    decode _ = Nothing

showBoard :: Board -> IO String
showBoard bd = do
    v <- Vec.freeze bd
    return $ intercalate "\n" $
        [headerFooter] ++
        zipWith doLine blankBoardLines (chunksOf 19 $ Vec.toList v) ++
        [headerFooter]
  where
    doSq _ (Just Black) = '●'
    doSq _ (Just White) = '℗'
    doSq bg _ = bg
    doLine :: String -> [Maybe Color] -> String
    doLine blankL l =
        zipWith doSq blankL ([n, n] ++ intercalate [n] (map (:[]) l) ++ [n, n])
    n = Nothing
    headerFooter = "  A B C D E F G H J K L M N O P Q R S T"
    blankBoardLines =
        [ "19┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐19"
        , "18├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤18"
        , "17├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤17"
        , "16├─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┤16"
        , "15├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤15"
        , "14├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤14"
        , "13├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤13"
        , "12├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤12"
        , "11├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤11"
        , "10├─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┤10"
        , " 9├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤9"
        , " 8├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤8"
        , " 7├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤7"
        , " 6├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤6"
        , " 5├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤5"
        , " 4├─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┤4"
        , " 3├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤3"
        , " 2├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤2"
        , " 1└─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘1"
        ]

readMb :: Read a => String -> Maybe a
readMb s = fmap fst . listToMaybe $ reads s

readCoord :: String -> Maybe (Int, Int)
readCoord (columnCh:rowStr) =
    let columnMb
          | 'A' <= columnCh && columnCh <= 'T' && columnCh /= 'I'
          = Just (ord columnCh - ord 'A' - if columnCh > 'I' then 1 else 0)
          | 'a' <= columnCh && columnCh <= 't' && columnCh /= 'i'
          = Just (ord columnCh - ord 'a' - if columnCh > 'i' then 1 else 0)
          | otherwise
          = Nothing
        rowMb = readMb rowStr
    in case (columnMb, rowMb) of
      (Just column, Just row) -> Just (column, 19 - row)
      _ -> Nothing
readCoord _ = Nothing

readColor :: String -> Maybe Color
readColor s = case s of
  "b" -> Just Black
  "B" -> Just Black
  "w" -> Just White
  "W" -> Just White
  _ -> Nothing

getMove :: IO (Maybe (Either Color Move))
getMove = do
    putStr "Your move: "
    hFlush stdout
    s <- getLine
    let unknown = do
            putStrLn "I could not read your move. Please try again (e.g. D4)."
            getMove
    case s of
      "q" -> return Nothing
      "quit" -> return Nothing
      "exit" -> return Nothing
      "eb" -> return $ Just $ Left Black
      "eB" -> return $ Just $ Left Black
      "ew" -> return $ Just $ Left White
      "eW" -> return $ Just $ Left White
      colorCh:coordStr -> case (readColor [colorCh], readCoord coordStr) of
        (Just color, Just (column, row)) ->
          return $ Just $ Right $ Move color column row
        _ -> unknown
      _ -> unknown

{-
playEngine :: Engine -> Board -> IO ()
playEngine e b = do
    showBoard b >>= putStrLn
    gotMove <- getMove
    print gotMove
    case gotMove of
      Just (Right move) -> do
          ePlayMove e move
          bPlayMove b move
          playEngine e b
      Just (Left color) -> do
          eGenMove e color
          let waitForAns = do
                  l <- hGetLine (eOutH e)
                  case l of
                    "= " -> waitForAns
                    '=':' ':coordSq -> do
                        let Just (column, row) = readCoord coordSq
                        return $ Move White column row
                    _ -> waitForAns
          move <- waitForAns
          print move
          bPlayMove b move
          playEngine e b
      _ -> return ()
    
playRestartingEngine :: Board -> IO ()
playRestartingEngine b = do
    showBoard b >>= putStrLn
    gotMove <- getMove
    print gotMove
    case gotMove of
      Just (Right move) -> bPlayMove b move >> playRestartingEngine b
      Just (Left color) -> do
          move <- withEngine $ \e -> do
              eSetBoard e b
              eGenMove e color
              let waitForAns = do
                      l <- hGetLine (eOutH e)
                      case l of
                        "= " -> waitForAns
                        '=':' ':coordSq -> do
                            let Just (column, row) = readCoord coordSq
                            return $ Move White column row
                        _ -> waitForAns
              waitForAns
          print move
          bPlayMove b move >> playRestartingEngine b
      _ -> return ()
-}

playRestartingEngine :: [Move] -> IO ()
playRestartingEngine moves = do
    b <- newBoard
    mapM_ (bPlayMove b) moves
    showBoard b >>= putStrLn
    gotMove <- getMove
    print gotMove
    case gotMove of
      Just (Right move) -> playRestartingEngine (moves ++ [move])
      Just (Left color) -> do
          move <- withEngine $ \e -> do
              eSetMoves e moves
              eGenMove e color
              let waitForAns = do
                      l <- hGetLine (eOutH e)
                      case l of
                        "= " -> waitForAns
                        '=':' ':coordSq -> do
                            let Just (column, row) = readCoord coordSq
                            return $ Move White column row
                        _ -> waitForAns
              waitForAns
          print move
          playRestartingEngine (moves ++ [move])
      _ -> return ()

withEngine :: (Engine -> IO a) -> IO a
withEngine f = withCreateProcess (
    (proc "my-go-engine" [])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}) $
    \(Just inH) (Just outH) (Just errH) _engProc -> f (Engine inH outH errH)

newBoard :: IO Board
newBoard = MVec.replicate (19 * 19) Nothing

bIndex :: Int -> Int -> Int
bIndex column row = 19 * row + column

bRead :: Board -> Int -> Int -> IO (Maybe Color)
bRead b column row = MVec.read b (bIndex column row)

bWrite :: Board -> Int -> Int -> Maybe Color -> IO ()
bWrite b column row v = MVec.write b (bIndex column row) v

bPlayMove :: Board -> Move -> IO ()
bPlayMove b m = bWrite b (mColumn m) (mRow m) (Just $ mColor m)

main :: IO ()
main = do
    args <- getArgs
    playRestartingEngine []

    -- withEngine $ \e -> playEngine e b
    {-
    withEngine $ \e -> forM_ args $ \arg -> do
        moves <- map parseMove . filter (";" `isPrefixOf`) . lines <$>
            readFile arg

        let color = case colorLine of
              "●" -> 10000
              "℗" -> 20000
              _ -> error "Failed to read whose turn it is."
            myBd = readBoard ls
        putStrLn colorLine
        putStrLn $ showBoard myBd
        evalAllMoves engIn engErr myBd color
    -}
