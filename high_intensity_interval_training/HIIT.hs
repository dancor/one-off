#include <h>

say :: String -> IO ()
say a = do
  (_exitCode, _out, _err) <- readProcessWithExitCode "espeak" [] a
  -- my espeak always says errors but works.  i guess we could check exit code
  -- at least.
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
