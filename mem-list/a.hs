main = interact (unlines . f . ("<<START>>" :) . lines)

f (x:y:rest) = (head (words x) ++ "\t" ++ y) : f (y:rest)
f _ = []
