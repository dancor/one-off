import System.Process

main = do
    (_, _, _, pId) <- runInteractiveProcess "sleep" ["19"] Nothing Nothing
    waitForProcess pId
