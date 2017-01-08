{-# language DeriveFunctor, FlexibleInstances, StandaloneDeriving, ExistentialQuantification #-}
module Rasa.Ext.Views 
  (
  -- getViews
  -- , split
  -- , single
   Dir(..), SplitRule(..), Views(..), Window, ViewInfo(..), SplitInfo(..), WindowF(Split, Single)) where

import Rasa.Ext

import Data.Profunctor.Unsafe
import Data.Coerce

import Data.Bifunctor.Fix
import Prelude hiding (Foldable, succ)
import Control.Lens
import Data.Default
import Data.Monoid
import Data.Functor.Foldable hiding (Fix)

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

data WindowF r a =
  Split Dir SplitInfo r r
    | Single ViewInfo a
    deriving (Show, Functor)

instance Bifunctor WindowF where
  bimap f _g (Split dir si x y) = Split dir si (f x) (f y)
  bimap _f g (Single vi a) = Single vi (g a)

newtype Window a = Window (Fix WindowF a) deriving Functor
-- type Window a = Fix WindowF a
-- type Window a = Fix WindowF a

-- split :: Dir -> SplitInfo -> Window a -> Window a -> Window a
-- split dir splitInfo (Window start) (Window end) = Window $ In $ Split dir splitInfo start end

-- single :: ViewInfo -> a -> Window a
-- single viewInfo contents = Window $ In $ Single viewInfo contents


newtype Flip p a b = Flip {unFlip :: p b a}

instance Bifunctor p => Bifunctor (Flip p) where
  bimap f g (Flip x) = Flip (bimap g f x)

instance Bifunctor p => Functor (Flip p a) where
  fmap = coerce (first :: (x -> y) -> p x a -> p y a)
    :: forall x y . (x -> y) -> Flip p a x -> Flip p a y

type instance Base (Fix p a) = Flip p a
instance Bifunctor p => Recursive (Fix p a) where
  project = Flip #. out
  cata f = f . Flip . first (cata f) . out

allTree :: Window Bool -> Bool
allTree (Window w) = cata alg w
  where
    alg (In (Split _ _ x y)) = x && y
    alg (In (Single _ x)) = x

data Views = Views
  { main :: Window Int
  }

instance Show Views where
  show _ = "Views"

-- instance Default Views where
--   def = Views $ split Hor (SplitInfo $ Percentage 0.5)
--                               (single (ViewInfo True) 0)
--                               $ split Vert (SplitInfo $ Percentage 0.5)
--                                   (single (ViewInfo False) 0)
--                                   (single (ViewInfo False) 0)

-- getViews :: Action Views
-- getViews = use ext
