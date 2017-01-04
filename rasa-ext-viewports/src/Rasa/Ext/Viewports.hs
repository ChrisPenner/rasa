{-# language DeriveFunctor #-}
module Rasa.Ext.Viewports (getViewports, Dir(..), SplitRule(..), Viewports(..), Window(..), SplitInfo(..)) where

import Rasa.Ext
import Control.Lens

import Data.Default

data SplitRule = 
  Percentage Double
  | FromStart Int
  | FromEnd Int
  deriving (Show, Eq)

data SplitInfo = SplitInfo
  { splitRule :: SplitRule
  } deriving Show

data Dir = Hor
         | Vert
         deriving (Show, Eq)

data Window a =
  Split Dir SplitInfo (Window a) (Window a)
    | Single a
    deriving (Show, Functor)

data Viewports = Viewports
  { main :: Window Int
  } deriving Show

instance Default Viewports where
  def = Viewports $ Split Hor (SplitInfo $ FromEnd 3)
                              (Single 0)
                              $ Split Vert (SplitInfo $ Percentage 0.7)
                                  (Single 0)
                                  (Single 0)

getViewports :: Action Viewports
getViewports = use ext
