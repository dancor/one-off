data Surreal = Surreal [Surreal] [Surreal] deriving (Eq)

instance Ord Surreal where
  x@(Surreal l _) <= y@(Surreal _ r) =
    not (any (y <=) l) && not (any (<= x) r)

main = do
  print $ zero <= one
  print $ one <= zero
  where
  zero = Surreal [] []
  one = Surreal [zero] []
