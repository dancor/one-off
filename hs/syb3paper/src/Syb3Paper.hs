{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Syb3Paper where

import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances

--------- gsize library code -----------
class Size a where gsize :: a -> Int

data SizeD a = SizeD { gsizeD :: a -> Int }

sizeProxy :: Proxy SizeD
sizeProxy = error "urk"

instance Size t => Sat (SizeD t) where
    dict = SizeD { gsizeD = gsize }

instance Data SizeD t => Size t where
    gsize t = 1 + sum (gmapQ sizeProxy (gsizeD dict) t)

--------- gsize client code -----------
instance Size a => Size [a] where
    gsize [] = 0
    gsize (x:xs) = gsize x + gsize xs

test = (gsize ['a', 'b'], gsize 'x')
-- Result = (2,1)
