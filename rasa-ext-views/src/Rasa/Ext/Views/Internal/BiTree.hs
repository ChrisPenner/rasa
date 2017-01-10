{-# language DeriveFunctor, DeriveTraversable, TypeFamilies  #-}
module Rasa.Ext.Views.Internal.BiTree where

import Data.Typeable
import Data.Functor.Foldable

data BiTree b l =
  Branch b (BiTree b l) (BiTree b l)
    | Leaf l
    deriving (Show, Functor, Traversable, Foldable, Typeable)

type instance Base (BiTree a b) = BiTreeF a b
instance Recursive (BiTree a b) where
  project (Leaf x) = LeafF x
  project (Branch s l r) = BranchF s l r

data BiTreeF b l r =
  BranchF b r r
    | LeafF l
    deriving (Show, Functor, Typeable)
