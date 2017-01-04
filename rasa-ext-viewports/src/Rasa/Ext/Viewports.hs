{-# language TemplateHaskell, DeriveFunctor #-}
module Rasa.Ext.Viewports (getViewports, Viewports(..), Window(..), SplitInfo(..)) where

import Rasa.Ext
import Control.Lens

import Data.Default

data SplitInfo = SplitInfo
  { splitPoint :: Double
  } deriving Show
makeLenses ''SplitInfo

data Window a =
  VSplit SplitInfo (Window a) (Window a) -- Left Right
    | HSplit SplitInfo (Window a) (Window a) -- Top Bottom
    | Single a
    deriving (Show, Functor)

data Viewports = Viewports
  { main :: Window Int
  } deriving Show

instance Default Viewports where
  def = Viewports $ HSplit (SplitInfo 0.5)
                              (Single 0)
                              $ VSplit (SplitInfo 0.5)
                                  (Single 0)
                                  (Single 0)

getViewports :: Action Viewports
getViewports = use ext
