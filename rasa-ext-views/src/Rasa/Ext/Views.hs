{-# language FlexibleInstances #-}
module Rasa.Ext.Views
  (
  getViews
  -- , split
  -- , single
  ,Dir(..), SplitRule(..), Window(..), Split(..), Views(..), View(..)
  , BiTreeF(..)
  ) where

import Rasa.Ext
import Rasa.Ext.Views.Internal.BiTree

import Control.Lens
import Data.Default
-- import Data.Monoid
import Data.Functor.Foldable

data SplitRule =
  Percentage Double
  | FromStart Int
  | FromEnd Int
  deriving (Show)

data Split = Split
  { dir :: Dir
  , splitRule :: SplitRule
  } deriving (Show)

data View = View
  { active :: Bool
  , bufIndex :: Int
  } deriving (Show)

data Dir = Hor
         | Vert
         deriving (Show)

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

data Views = Views
  { main :: Window Split View
  }

instance Show Views where
  show _ = "Views"

instance Default Views where
  def = Views $ branch (Split Hor $ Percentage 0.5)
                              (leaf $ View True 0)
                              $ branch (Split Vert $ Percentage 0.5)
                                  (leaf $ View False 0)
                                  (leaf $ View False 0)

getViews :: Action Views
getViews = use ext
