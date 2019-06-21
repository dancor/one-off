import Graphics.Gloss.Interface.IO.Game

winW, winH :: Int
winW = 700
winH = 700

gameState = ()

-- total width:  cellW * colNum + barW * (colNum + 1)
-- total height: cellH * rowNum + barH * (rowNum + 1)
grid :: Int -> Int -> Float -> Float -> Float -> Float -> Picture
grid colNum rowNum cellW cellH barW barH = pictures $
    [ translate 0
          ((fromIntegral y - fromIntegral rowNum / 2) * (cellH + barH)) $
      rectangleSolid totalW barH
    | y <- [0..rowNum]
    ] ++
    [ translate
          ((fromIntegral x - fromIntegral colNum / 2) * (cellW + barW))
          0 $
      rectangleSolid barW totalH
    | x <- [0..colNum]
    ] ++
    [ translate (0.5 * cellW - 0.0 * barW) (0.5 * cellH - 0.0 * barH) $ 
      circleSolid (1 + 2 * barW) ]
  where
    totalW = cellW * fromIntegral colNum + barW * (fromIntegral $ colNum + 1)
    totalH = cellH * fromIntegral rowNum + barH * (fromIntegral $ rowNum + 1)

renderGameState () = do
    let cellSize = fromIntegral $ winW `div` 21
        barSize = fromIntegral $ winW `div` 350
    print $ cellSize
    print $ barSize
    -- return $ Scale (fromIntegral winW) (fromIntegral winH) $
    --    translate 0 (-0) $ grid 19 19 0.0025 0.0025 0.05 0.05
    --return . translate 0 (negate $ fromIntegral winW / 2) $
    return . translate 0 0 $
        grid 19 19 cellSize cellSize barSize barSize

-- handleEvent (EventKey (MouseButton () = do
handleEvent _ () = return ()

handleTime _ () = return ()

main :: IO ()
main = do
    playIO (InWindow "bubugo" (winW, winH) (0, 700)) orange 1 gameState
        renderGameState handleEvent handleTime
