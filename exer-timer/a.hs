import Control.Monad
import Data.Char
import System.IO
import qualified Control.Concurrent as Conc

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    putStr $ "Do Ex1 warm-up and set 1 and hit enter."
    _ <- getLine
    countDown 90
    finishSet 1

finishSet :: Int -> IO ()
finishSet n = do
    putStr $ "Do Ex" ++ show n ++ " set 2 and hit enter."
    _ <- getLine
    countDown 90
    if n == 6 then putStr "Finish with Ex6 set 3. Good job!" else do
        putStr $ "Do Ex" ++ show n ++ " set 3, Ex" ++ show n ++ 
            " warm-up, set 1and hit enter."
        _ <- getLine
        countDown 90
        finishSet (n + 1)

checkForEnter :: IO Bool
checkForEnter = do
  r <- hReady stdin
  if r
    then do
      c <- getChar
      if c == '\n' then return True else checkForEnter
    else return False

countDown :: Int -> IO ()
countDown 0 = return ()
countDown n = do
  putStr $ if n `mod` 30 == 0 then "\n" ++ show n else "."
  gotEnter <- checkForEnter
  unless gotEnter $ Conc.threadDelay 1000000 >> countDown (n - 1)
