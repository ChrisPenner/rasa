module Rasa.Ext.Views.Internal.Actions
  ( scrollBy
  , hSplit
  , vSplit
  , closeView
  ) where

import Rasa.Ext
import qualified Rasa.Ext.Views.Internal.Types as T

import Control.Lens

scrollBy :: Int -> Action ()
scrollBy amt = T.windows.traverse.filtered (view T.active). T.scrollPos += amt

hSplit :: Action ()
hSplit = T.windows %= T.hSplit

vSplit :: Action ()
vSplit = T.windows %= T.vSplit

closeView :: Action ()
closeView = T.windows %= T.closeView

-- focusViewLeft :: Action ()
-- focusViewLeft = T.windows %= T.focusView L
