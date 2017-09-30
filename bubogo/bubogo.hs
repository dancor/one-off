{-# LANGUAGE ParallelListComp #-}
#include <h>

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R

-- play b q16
-- genmove w

-- Using magic numbers since I'm bad at repa?
emptySq, black, white :: Int
emptySq = 0
black = 10000
white = 20000

goBdZ = R.Z R.:. 19 R.:. 19

type BoardOf a = R.Array R.U R.DIM2 a

type Board = BoardOf Int

emptyBoard :: Board
emptyBoard = R.fromList goBdZ (repeat 0)

getEval :: Handle -> IO String
getEval engErr = do
    l <- hGetLine engErr
    if "1: " `isPrefixOf` l
      then return $ takeWhile (/= '[') (words l !! 3)
      else do
        -- putStrLn l
        getEval engErr

-- printBoard bd = [

colorToStr, colStr, rowStr :: Int -> String
colorToStr 10000 = "b"
colorToStr 20000 = "w"
colorToStr _ = error "colorToStr"
colStr col = [chr (col + ord 'A' + if col >= 8 then 1 else 0)]
rowStr row = show (row + 1)

switchColor :: Int -> Int
switchColor 10000 = 20000
switchColor 20000 = 10000
switchColor _ = error "switchColor"

evalAllMoves :: Handle -> Handle -> Board -> Int -> IO ()
evalAllMoves engIn engErr bd color = do
    let stoneList = 
            [ (x,y,myC)
            | y <- [0..18::Int]
            , x <- [0..18::Int]
            , let myC = bd R.! (R.Z R.:. y R.:. x)
            , myC == 10000 || myC == 20000
            ]
        setBoard :: IO ()
        setBoard = do
            hPutStrLn engIn "clear_board"
            forM_ stoneList $ \(x,y,c) -> hPutStrLn engIn
                ("play " ++ colorToStr (c :: Int) ++ " " ++
                colStr (x :: Int) ++ rowStr (y :: Int))
        doSq x y = do
            setBoard
            hPutStrLn engIn $
                "play " ++ colorToStr color ++ " " ++ colStr x ++ rowStr y
            hPutStrLn engIn $ "genmove " ++ colorToStr (switchColor color)
            hFlush engIn
            eval <- getEval engErr
            putStr $ show $ min 9 $ floor $ read eval / 10
    forM_ [0..18] $ \y -> do
        forM_ [0..18] $ \x -> case bd R.! (R.Z R.:. y R.:. x) of
            10000 -> putStr "●"
            20000 -> putStr "℗"
            _ -> doSq x y
        putStrLn ""

readBoard :: [String] -> Board
readBoard =
    R.fromList goBdZ . concat .
    map (map decode . take 19 . everyOther . drop 2) . 
    take 19 . tail
  where
    everyOther (x:_:xs) = x : everyOther xs
    everyOther _ = []
    decode '●' = black
    decode '℗' = white
    decode _ = emptySq

showBoard :: Board -> String
showBoard bd =
    intercalate "\n" $
        [headerFooter] ++
        zipWith doLine blankBoardLines ls ++
        [headerFooter]
  where
    ls = chunksOf 19 $ R.toList bd
    doLine blankL l =
        zipWith (\b e -> if e == ' ' then b else e)
            blankL
            ("  " ++ intercalate " " (map encode l) ++ "  ")
    encode 10000 = "●"
    encode 20000 = "℗"
    encode 0 = " "
    encode i = [chr $ ord '0' + min 9 (i `div` 100)]
    headerFooter = "  A B C D E F G H J K L M N O P Q R S T"
    blankBoardLines =
        [ " 1┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐1"
        , " 2├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤2"
        , " 3├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤3"
        , " 4├─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┤4"
        , " 5├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤5"
        , " 6├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤6"
        , " 7├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤7"
        , " 8├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤8"
        , " 9├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤9"
        , "10├─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┤10"
        , "11├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤11"
        , "12├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤12"
        , "13├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤13"
        , "14├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤14"
        , "15├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤15"
        , "16├─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┼─┼─┼─·─┼─┼─┤16"
        , "17├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤17"
        , "18├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤18"
        , "19└─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘19"
        ]

main :: IO ()
main = do
    args <- getArgs
    withCreateProcess (
        (proc "my-go-engine" [])
        {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}) $
        \(Just engIn) _ (Just engErr) _engProc -> forM_ args $ \arg -> do
            colorLine:ls <- lines <$> readFile arg
            let color = case colorLine of
                  "●" -> 10000
                  "℗" -> 20000
                  _ -> error "Failed to read whose turn it is."
                myBd = readBoard ls
            putStrLn colorLine
            putStrLn $ showBoard myBd
            evalAllMoves engIn engErr myBd color
            --hPutStrLn engIn "quit"
            --hFlush engIn
