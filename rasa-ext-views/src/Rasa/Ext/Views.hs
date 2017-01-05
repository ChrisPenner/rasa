{-# language DeriveFunctor, FlexibleInstances #-}
module Rasa.Ext.Views (getViews, split, single, Dir(..), SplitRule(..), Views(..), Window(..), ViewInfo(..), SplitInfo(..), WindowF(Split, Single)) where

import Rasa.Ext

import Data.Functor.Foldable
import Prelude hiding (Foldable, succ)
import Control.Lens
import Data.Default

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

type Window a = Fix (WindowF a)

-- instance Bifunctor WindowF where
--   first f (Single viewInfo a) = Single viewInfo (f a)
--   first f (Split dir splitInfo start end) = Split dir splitInfo (first f start) (first f end)
--   second = fmap

valMap :: (a -> b) -> Window a -> Window b
valMap f = cata alg
  where alg  (Single viewInfo a) = refix $ Single viewInfo (f a)
        alg splt = splt

split :: Show a => Dir -> SplitInfo -> Window a -> Window a -> Window a
split dir splitInfo start end = Fix (Split dir splitInfo start end)

single :: Show a => ViewInfo -> a -> Window a
single viewInfo contents = Fix (Single viewInfo contents)

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
