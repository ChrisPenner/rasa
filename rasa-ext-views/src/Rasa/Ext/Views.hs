{-# language FlexibleInstances, TemplateHaskell #-}
module Rasa.Ext.Views
  (
  getViews
  , rotate
  , bufIndex
  , splitRule
  , active
  , closeBy
  , moveRight
  ,Dir(..), SplitRule(..), Window(..), Split(..), Views(..), View(..)
  , BiTreeF(..)
  ) where

import Rasa.Ext
import Rasa.Ext.Views.Internal.BiTree

import Control.Lens
import Data.Default
import Data.Functor.Foldable

data SplitRule =
  Percentage Double
  | FromStart Int
  | FromEnd Int
  deriving (Show)

data Dir = Hor
         | Vert
         deriving (Show)

data Split = Split
  { _dir :: Dir
  , _splitRule :: SplitRule
  } deriving (Show)

makeLenses ''Split

data View = View
  { _active :: Bool
  , _bufIndex :: Int
  } deriving (Show)

makeLenses ''View

instance Default View where
  def = View True 0

branch :: b -> Window b l -> Window b l -> Window b l
branch b (Window start) (Window end) = Window $ Fix $ Branch b start end

leaf :: b -> Window a b
leaf contents = Window $ Fix $ Leaf contents

newtype Window a b = Window
  { getWin :: Fix (BiTreeF a b)
  }

instance Functor (Window a) where
  fmap f (Window (Fix (Leaf vw))) = leaf $ f vw
  fmap f (Window (Fix (Branch s a b))) = branch s (f <$> Window a) (f <$> Window b)

instance Traversable (Window a) where
  traverse f (Window (Fix (Leaf vw))) = Window . Fix . Leaf <$> f vw
  traverse f (Window (Fix (Branch s a b))) = mkBranch <$> (getWin <$> traverse f (Window a)) <*> (getWin <$> traverse f (Window b))
    where mkBranch a' b' = Window $ Fix $ Branch s a' b'

instance Foldable (Window a) where
  foldMap f (Window (Fix (Leaf vw))) = f vw
  foldMap f (Window (Fix (Branch _ a b))) = foldMap f (Window a) `mappend` foldMap f (Window b)

data Views = Views
  { main :: Window Split View
  }

instance Show Views where
  show _ = "Views"

instance Default Views where
  def = Views $ branch (Split Vert $ Percentage 0.5)
                              (leaf $ View True 0)
                              $ branch (Split Hor $ Percentage 0.5)
                                  (leaf $ View False 1)
                                  (leaf $ View False 1)


rotate :: Window Split View -> Window Split View
rotate (Window w) = Window $ cata alg w
  where alg l@(Leaf _) = Fix l
        alg (Branch sp s e) = Fix (Branch (sp & dir %~ rotDir) s e)
        rotDir Hor = Vert
        rotDir Vert = Hor

closeBy :: (View -> Bool) -> Window Split View -> Window Split View
closeBy p  (Window w) = Window $ zygo par alg w
  where
    par (Leaf vw) = not $ p vw
    par (Branch _ l r) = l || r
    alg (Leaf vw) = Fix (Leaf vw)
    alg (Branch sp (keepLeft, al) (keepRight, ar))
      | keepLeft && keepRight = Fix $ Branch sp al ar
      | keepLeft = al
      | keepRight = ar
      | otherwise = Fix (Leaf def)

moveRight :: Window Split View -> Window Split View
moveRight (Window w) = Window $ zygo par alg w
  where
    par (Leaf vw) = vw^.active
    par (Branch (Split Hor _) l r) = l || r
    par (Branch (Split Vert _) _ r) = r
    alg (Leaf vw) = Fix $ Leaf vw
    alg (Branch sp@(Split Hor _) (_, al) (_, ar)) = Fix $ Branch sp al ar
    alg (Branch sp@(Split Vert _) (fromLeft, al) (_, ar)) =
      Fix $ Branch sp al right
        where right = if fromLeft
                         then Window ar & taking 1 traverse . active .~ True & getWin
                         else ar

getViews :: Action Views
getViews = use ext
