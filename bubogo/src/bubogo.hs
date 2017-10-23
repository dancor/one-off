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

data Coord = Coord
  { cColumn :: !Int
  , cRow    :: !Int
  } deriving Show

data Move = Move
  { mColor :: !Color
  , mCoord :: !Coord
  } deriving Show

type BoardOf a = MVec.IOVector a

type Board = BoardOf (Maybe Color)

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

otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black

ePut :: Engine -> String -> IO ()
ePut e = hPutStrLn (eInH e)
--ePut e s = putStrLn s >> hPutStrLn (eInH e) s

ePlayMove :: Engine -> Move -> IO ()
ePlayMove e (Move color (Coord column row)) = do
    when (color == White) $ ePut e "play pass"
    ePut e $
        "play " ++ colorLtr color ++ " " ++ columnStr column ++ rowStr row
    when (color == Black) $ ePut e "play pass"

eSetBoard :: Engine -> Board -> IO ()
eSetBoard e b = do
    ePut e "clear_board"
    forM_ allCoords $ \c -> do
        sqHas <- bRead b c
        case sqHas of
          Just color -> ePlayMove e (Move color c)
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
    let doSq c = do
            eSetBoard e b
            ePlayMove e (Move color c)
            eGenMove e (otherColor color)
            eval <- getEval $ eErrH e
            putStr $ show $ 9 - min 9 (floor $ read eval / 10)
    forM_ [0..18] $ \row -> do
        forM_ [0..18] $ \column -> do
            let c = Coord column row
            sqHas <- bRead b c
            case sqHas of
              Just Black -> putStr "●"
              Just White -> putStr "℗"
              _ -> doSq c
        putStrLn ""

readBoard :: Board -> [String] -> IO ()
readBoard b = zipWithM_ readRow [0..18] . take 19 . tail
  where
    readRow row =
        zipWithM_ (readSq row) [0..18] . take 19 . everyOther . drop 2
    readSq row column = bWrite b (Coord column row) . decode
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

readCoord :: String -> Maybe Coord
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
      (Just column, Just row) -> Just $ Coord column (19 - row)
      _ -> Nothing
readCoord _ = Nothing

coordParser :: Psec.Parser Coord
coordParser = do
    columnCh <- Psec.anyChar
    let columnMb
            | 'A' <= columnCh && columnCh <= 'T' && columnCh /= 'I'
            = Just $ ord columnCh - ord 'A' - if columnCh > 'I' then 1 else 0
            | 'a' <= columnCh && columnCh <= 't' && columnCh /= 'i'
            = Just $ ord columnCh - ord 'a' - if columnCh > 'i' then 1 else 0
            | otherwise
            = Nothing
    case columnMb of
      Just column -> do 
        rowPreFlip <- read <$> Psec.many1 Psec.digit
        return $ Coord column (19 - rowPreFlip)
      Nothing -> fail "Bad column"

colorParser :: Psec.Parser Color
colorParser = Psec.choice
  [ Psec.char 'b' >> return Black
  , Psec.char 'B' >> return Black
  , Psec.char 'w' >> return White
  , Psec.char 'W' >> return White
  ]

data GetMove
    = GotQuit
    | EngineMove Color
    | Got Move
    | GotUndo
    deriving Show

moveParser :: Color -> Psec.Parser GetMove
moveParser defColor = Psec.choice
  [ Psec.try $ Psec.string "undo" >> return GotUndo          <* Psec.eof
  , Psec.try $ Psec.string "u"    >> return GotUndo          <* Psec.eof
  , Psec.try $ Got <$> liftM2 Move colorParser coordParser   <* Psec.eof
  , Psec.try $ (Got . Move defColor) <$> coordParser         <* Psec.eof
  , Psec.try $ Psec.char 'e' >> EngineMove <$> colorParser   <* Psec.eof
  , Psec.try $ Psec.char 'e' >> return (EngineMove defColor) <* Psec.eof
  , Psec.try $ Psec.string "quit" >> return GotQuit          <* Psec.eof
  , Psec.try $ Psec.string "q"    >> return GotQuit          <* Psec.eof
  ,            Psec.string "exit" >> return GotQuit          <* Psec.eof
  ]

getMove :: Color -> IO [GetMove]
getMove defColor = do
    putStr $ "Your move (" ++ show defColor ++ "): "
    hFlush stdout
    moveStrs <- words <$> getLine
    case sequence (map (Psec.parse (moveParser defColor) "") moveStrs) of
      Right ret -> return ret
      _ -> do
        putStrLn "I could not read your move. Please try again (e.g. D4)."
        getMove defColor
      
playRestartingEngine :: Color -> [Move] -> [GetMove] -> IO ()
playRestartingEngine color moves queuedGots = case queuedGots of
  [] -> do
    b <- newBoardOf Nothing
    mapM_ (bPlayMove b) moves
    showBoard b >>= putStrLn
    getMove color >>= playRestartingEngine color moves
  (queuedGot:rest) -> case queuedGot of
    Got move -> playRestartingEngine
      (otherColor $ mColor move) (moves ++ [move]) rest
    EngineMove eColor -> do
      moveMb <- withEngine $ \e -> do
          eSetMoves e moves
          eGenMove e eColor
          let waitForAns = do
                  l <- hGetLine (eOutH e)
                  case l of
                    "= " -> waitForAns
                    "= resign" -> do
                        putStrLn "Engine resigns."
                        return Nothing
                    '=':' ':s -> do
                        case Psec.parse coordParser "" s of
                          Right c -> return $ Just $ Move eColor c
                          _ -> do
                              putStrLn "Could not understand engine move."
                              return Nothing
                    _ -> waitForAns
          waitForAns
      playRestartingEngine (otherColor eColor)
          (moves ++ maybeToList moveMb) rest
    GotUndo -> case moves of
      [] -> playRestartingEngine Black [] rest
      _ ->
        let l = length moves - 1 in
        playRestartingEngine (mColor $ moves !! l) (take l moves) rest
    GotQuit -> return ()

withEngine :: (Engine -> IO a) -> IO a
withEngine f = withCreateProcess (
    (proc "my-go-engine" [])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}) $
    \(Just inH) (Just outH) (Just errH) _engProc -> f (Engine inH outH errH)

newBoardOf :: a -> IO (BoardOf a)
newBoardOf = MVec.replicate (19 * 19)

bIndex :: Coord -> Int
bIndex (Coord column row) = 19 * row + column

bRead :: BoardOf a -> Coord -> IO a
bRead b = MVec.read b . bIndex

bWrite :: BoardOf a -> Coord -> a -> IO ()
bWrite b = MVec.write b . bIndex

-- It is assumed that the Board has Color at Coord.
getChain :: Board -> Color -> Coord -> IO (BoardOf Bool)
getChain b color c = do
    chain <- newBoardOf False
    growChain b chain color c
    return chain

growChain :: Board -> BoardOf Bool -> Color -> Coord -> IO ()
growChain b chain color c = do
    alreadySeen <- bRead chain c
    unless alreadySeen $ do
        v <- bRead b c
        when (v == Just color) $ do
            bWrite chain c True
            mapM_ (growChain b chain color) $ coordNeighbors c

coordNeighbors :: Coord -> [Coord]
coordNeighbors (Coord column row) = concat
    [ if column ==  0 then [] else [Coord (column - 1) row]
    , if column == 18 then [] else [Coord (column + 1) row]
    , if row    ==  0 then [] else [Coord column (row - 1)]
    , if row    == 18 then [] else [Coord column (row + 1)]
    ] 

orM :: [IO Bool] -> IO Bool
orM [] = return False
orM (x:xs) = do
    r <- x
    if r then return True else orM xs

anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM f xs = orM (map f xs)

allCoords :: [Coord]
allCoords = [Coord column row | column <- [0..18], row <- [0..18]]

chainCoords :: BoardOf Bool -> IO [Coord]
chainCoords chain = catMaybes <$> mapM isIn allCoords
  where
    isIn c = do
        inChain <- bRead chain c
        return $ if inChain then Just c else Nothing

chainHasLiberties :: Board -> BoardOf Bool -> IO Bool
chainHasLiberties b chain = anyM cHasLib =<< chainCoords chain
  where cHasLib = anyM (fmap isNothing . bRead b) . coordNeighbors

captureChain :: Board -> BoardOf Bool -> IO ()
captureChain b chain = mapM_ (\c -> bWrite b c Nothing) =<< chainCoords chain

tryCapture :: Board -> Color -> Coord -> IO ()
tryCapture b capturedColor c = do
    v <- bRead b c
    when (v == Just capturedColor) $ do
        chain <- getChain b capturedColor c
        hasLib <- chainHasLiberties b chain
        unless hasLib $ captureChain b chain

bPlayMove :: Board -> Move -> IO ()
bPlayMove b (Move color c) = do
    bWrite b c (Just color)
    mapM_ (tryCapture b (otherColor color)) $ coordNeighbors c

main :: IO ()
main = do
    args <- getArgs
    playRestartingEngine Black [] []
