#include <h>

-- XX shell escape a
-- actually just use stdin
say :: String -> IO ()
say a = do
  forkIO . HSH.runIO $ "echo " ++ a ++ " | espeak 2> /dev/null"
  return ()

printSay :: String -> IO ()
printSay a = putStrLn a >> say a

doInterval :: String -> Int -> IO ()
doInterval whatToSay durationInSeconds =
  when (durationInSeconds > 0) $
    zipWithM_ (\ io n -> io >> threadDelay (n * 1000^2)) ios durations
  where
  ios = printSay whatToSay : reverse (map (say . show) [1..numSingles])
  numSingles = min 3 (durationInSeconds - 1)
  durations = (durationInSeconds - numSingles) : repeat 1


doRoundN :: Int -> Int -> Int -> IO ()
doRoundN workSeconds restSeconds n = do
  doInterval ("start" ++ show n) workSeconds
  doInterval ("rest" ++ show n) restSeconds

main :: IO ()
main = do
  [numRoundsToDo, workSeconds, restSeconds] <- map read <$> getArgs
  mapM_ (doRoundN workSeconds restSeconds) [1..numRoundsToDo]
  printSay "bye"
