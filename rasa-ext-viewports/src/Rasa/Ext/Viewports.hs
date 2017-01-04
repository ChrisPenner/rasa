{-# language TemplateHaskell, DeriveFunctor #-}
module Rasa.Ext.Viewports (getViewports, Dir(..), Viewports(..), Window(..), SplitInfo(..)) where

import Rasa.Ext
import Control.Lens

import Data.Default

data SplitInfo = SplitInfo
  { splitPoint :: Double
  } deriving Show
makeLenses ''SplitInfo

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
  def = Viewports $ Split Hor (SplitInfo 0.5)
                              (Single 0)
                              $ Split Vert (SplitInfo 0.5)
                                  (Single 0)
                                  (Single 0)

getViewports :: Action Viewports
getViewports = use ext
