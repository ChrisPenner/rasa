{-# language
    OverloadedStrings
#-}
module Rasa.Ext.Views.Internal.ActiveBar
  ( activeBar
  ) where

import Rasa.Ext

import Rasa.Ext.Views.Internal.Widgets
import Rasa.Ext.Views.Internal.Actions
import Data.Default
import qualified Yi.Rope as Y

activeBar :: Action ()
activeBar = onEveryNewBuffer_ . addBottomBar $ do
  enabled <- isFocused
  if enabled
     then Just <$> getActiveBar
     else return Nothing
  where
    getActiveBar :: BufAction RenderInfo
    getActiveBar = return ("-----", [Span (Range (Coord 0 0) (Coord 0 5)) (fg Red)])
