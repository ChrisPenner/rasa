{-# language DeriveFunctor, FlexibleInstances, StandaloneDeriving, ExistentialQuantification #-}
module Rasa.Ext.Views
  (
  getViews
  -- , split
  -- , single
   ,Dir(..), SplitRule(..), Views(..), Window, ViewInfo(..)) where

import Rasa.Ext

import Control.Lens
import Data.Default
-- import Data.Monoid
import Data.Functor.Foldable

data SplitRule =
  Percentage Double
  | FromStart Int
  | FromEnd Int
  deriving (Show)

data SplitInfo = SplitInfo
  { splitRule :: SplitRule
  } deriving (Show)

data ViewInfo = ViewInfo
  { active :: Bool
  } deriving (Show)

data Dir = Hor
         | Vert
         deriving (Show)

data WindowF a r =
  Split Dir SplitInfo r r
    | Single ViewInfo a
    deriving (Functor)

split :: Dir -> SplitInfo -> Window a -> Window a -> Window a
split dir splitInfo (Window start) (Window end) = Window $ Fix $ Split dir splitInfo start end

single :: ViewInfo -> a -> Window a
single viewInfo contents = Window $ Fix $ Single viewInfo contents

newtype Window a = Window
  { getWin :: Fix (WindowF a)
  }

instance Functor Window where
  fmap f (Window (Fix (Single vw a))) = single vw (f a)

data Views = Views
  { main :: Window Int
  }

instance Show Views where
  show _ = "Views"

instance Default Views where
  def = Views $ split Hor (SplitInfo $ Percentage 0.5)
                              (single (ViewInfo True) 0)
                              $ split Vert (SplitInfo $ Percentage 0.5)
                                  (single (ViewInfo False) 0)
                                  (single (ViewInfo False) 0)

getViews :: Action Views
getViews = use ext
