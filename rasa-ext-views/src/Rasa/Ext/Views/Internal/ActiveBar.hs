{-# language
    OverloadedStrings
#-}
module Rasa.Ext.Views.Internal.ActiveBar
  ( ActiveBar(..)
  ) where

import Rasa.Ext

import Rasa.Ext.Views.Internal.Actions
import Data.Default
import qualified Yi.Rope as Y

data ActiveBar = ActiveBar

instance Renderable ActiveBar where
  render width _ _ _ = return . Just $ (Y.replicate width "-" , [Span (Range (Coord 0 0) (Coord 0 width)) (fg Red)])
