import Graphics.Gloss.Interface.IO.Game

winW, winH :: Int
winW = 700
winH = 700

gameState = ()

renderGameState () = do
    return Blank

-- handleEvent (EventKey (MouseButton () = do
handleEvent _ () = return ()

handleTime _ () = return ()

main :: IO ()
main = do
    playIO (InWindow "bubugo" (winW, winH) (0, 700)) black 1 gameState
        renderGameState handleEvent handleTime
