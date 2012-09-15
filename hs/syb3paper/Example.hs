{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

module Example where

import Data.Typeable

--------- SYB library code -----------
data Proxy (a :: * -> *)

class Sat a where { dict :: a }

class (Typeable a, Sat (ctx a))
      => Data ctx a where
    gmapQ :: Proxy ctx
          -> (forall b. Data ctx b => b -> r)
          -> a -> [r]

instance Sat (cxt Char) => Data cxt Char where
    gmapQ _ f n = []

instance (Sat (cxt [a]), Data cxt a)
         => Data cxt [a] where
    gmapQ _ f [] = []
    gmapQ _ f (x:xs) = [f x, f xs]

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

