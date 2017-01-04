{-# language DeriveFunctor #-}
module Rasa.Ext.Views (getViews, Dir(..), SplitRule(..), Views(..), Window(..), ViewInfo(..), SplitInfo(..)) where

import Rasa.Ext
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

data Window a =
  Split Dir SplitInfo (Window a) (Window a)
    | Single ViewInfo a
    deriving (Show, Functor)

data Views = Views
  { main :: Window Int
  } deriving Show

instance Default Views where
  def = Views $ Split Hor (SplitInfo $ FromEnd 3)
                              (Single (ViewInfo True) 0)
                              $ Split Vert (SplitInfo $ Percentage 0.7)
                                  (Single (ViewInfo False) 0)
                                  (Single (ViewInfo False) 0)

getViews :: Action Views
getViews = use ext
