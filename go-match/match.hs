#include <h>

e1Cmd = proc "./leelaz" ["--gtp", "--cpu-only", "--noponder",
    "--playouts", "1", "--weights", "weights/elf.gz"]

e2Cmd = proc "./leelaz" ["--gtp", "--cpu-only", "--noponder",
    "--playouts", "1", "--weights", "/home/danl/i/lizzie/leela-zero/autogtp/networks/2da87ea8da0f54e87b70159e6bb82811b61d1c31091b6e019fbe62aeaa803b9c.gz"]

launch eCmd = do
    (Just eIn, Just eOut, Just eErr, _ePid) <- createProcess eCmd {
        std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe}
    Conc.forkIO $ devNull eErr
    return (eIn, eOut)

devNull h = do
    eof <- hIsEOF h
    unless eof $ hGetLine h >> devNull h

ePutGet (hIn, hOut) l = do
    hPutStrLn hIn l
    hFlush hIn
    r <- hGetLine hOut
    "" <- hGetLine hOut
    return r

eAnswerless e l = do
    "= " <- ePutGet e l
    return ()

ePlay e color l = eAnswerless e $ "play " ++ colorStr color ++ " " ++ l

data Color = Black | White

colorStr Black = "black"
colorStr White = "white"

colorLtr Black = "b"
colorLtr White = "w"

otherColor Black = White
otherColor White = Black

tournament e1 e2 totalNumToPlay e1Wins e2Wins = do
    print $ "SCORE: " ++ show e1Wins ++ " to " ++ show e2Wins
    when (e1Wins + e2Wins < totalNumToPlay) $ do
        let (e1Color, isE1Turn) = if even (e1Wins + e2Wins)
              then (Black, True)
              else (White, False)
        res <- gamePlays e1 e2  e1Color isE1Turn
        let (e1Wins', e2Wins') = if res
              then (e1Wins + 1, e2Wins)
              else (e1Wins, e2Wins + 1)
        tournament e1 e2 totalNumToPlay e1Wins' e2Wins'

gamePlays e1 e2 e1Color isE1Turn = do
    let e2Color = otherColor e1Color
        (eGen, genColor, eNote) =
            if isE1Turn then (e1, e1Color, e2) else (e2, e2Color, e1)
    '=':' ':sq <- ePutGet eGen $ "genmove " ++ colorStr genColor
    case sq of
      "resign" -> do
        putStrLn ""
        eAnswerless e1 "clear_board"
        eAnswerless e2 "clear_board"
        return (not isE1Turn)  -- True if e1 won
      _ -> do
        putStr $ colorLtr genColor ++ sq ++ " "
        hFlush stdout
        ePlay eNote genColor sq
        gamePlays e1 e2 e1Color (not isE1Turn)

main = do
    e1 <- launch e1Cmd
    e2 <- launch e2Cmd
    tournament e1 e2 1000 0 0
