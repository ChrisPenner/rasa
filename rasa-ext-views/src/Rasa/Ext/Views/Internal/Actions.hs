module Rasa.Ext.Views.Internal.Actions
  ( scrollBy
  ) where

import Rasa.Ext
import Rasa.Ext.Views.Internal.Types

import Control.Lens

scrollBy :: Int -> Action ()
scrollBy amt = windows.traverse.filtered (view active).scrollPos += amt
