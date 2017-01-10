{-# language DeriveFunctor, DeriveTraversable, TypeFamilies  #-}
module Rasa.Ext.Views.Internal.BiTree where

import Data.Typeable
import Data.Bifunctor
import Data.Functor.Foldable

data BiTree b l =
  Branch b (BiTree b l) (BiTree b l)
    | Leaf l
    deriving (Show, Typeable, Functor, Traversable, Foldable)

instance Bifunctor BiTree where
  bimap _ g (Leaf x) = Leaf (g x)
  bimap f g (Branch b l r) = Branch (f b) (bimap f g l) (bimap f g r)

data BiTreeF b l r =
  BranchF b r r
    | LeafF l
    deriving (Show, Functor, Typeable)

type instance Base (BiTree a b) = BiTreeF a b
instance Recursive (BiTree a b) where
  project (Leaf x) = LeafF x
  project (Branch s l r) = BranchF s l r

instance Corecursive (BiTree a b) where
  embed (BranchF sp x xs) = Branch sp x xs
  embed (LeafF x) = Leaf x
