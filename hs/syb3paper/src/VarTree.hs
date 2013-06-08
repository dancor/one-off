{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
-}

module VarTree where

import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances

{-
import Data.Generics.SYB.WithClass.Derive

data Foo = Foo Int

$(derive [''Foo])
-}

class VarTree a where
    varTree :: a -> [String]

data VarTreeD a = VarTreeD {
    varTreeD :: a -> [String]
    }

varTreeProxy :: Proxy VarTreeD
varTreeProxy = error "VarTreeProxy should never be evaluated."

instance (VarTree a) => Sat (VarTreeD a) where
    dict = VarTreeD
        { varTreeD = varTree
        }

instance (Data VarTreeD a) => VarTree a where
    varTree var = 
        show constr :
        map (' ':) (concatMap wrapSubVar $
            gmapQ varTreeProxy (varTreeD dict) var)
      where
        constr = toConstr varTreeProxy var
        wrapSubVar = id

instance VarTree Char where varTree = (:[]) . show
instance VarTree String where varTree = (:[]) . show

instance (VarTree a) => VarTree [a] where
    varTree [] = ["[]"]
    varTree vars = concat (zipWith
        (\ varPunc varLines -> zipWith (:) (varPunc : repeat ' ') varLines)
        ('[' : repeat ',')
        (map varTree vars)
        ) ++ ["]"]

pp :: (VarTree a) => a -> IO ()
pp = putStr . unlines . varTree
