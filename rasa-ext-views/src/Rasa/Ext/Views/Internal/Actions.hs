module Rasa.Ext.Views.Internal.Actions where

import Rasa.Ext
import Rasa.Ext.Views.Internal.Views as V

import Control.Lens

refocusView :: Action ()
refocusView = V.windows %= V.refocusView

focusViewLeft :: Action ()
focusViewLeft = V.windows %= V.focusViewLeft

focusViewRight :: Action ()
focusViewRight = V.windows %= V.focusViewRight

focusViewAbove :: Action ()
focusViewAbove = V.windows %= V.focusViewAbove

focusViewBelow :: Action ()
focusViewBelow = V.windows %= V.focusViewBelow

closeInactive :: Action ()
closeInactive = V.windows %= V.closeBy (not . view active)

hSplit :: Action ()
hSplit = V.windows %= V.hSplit

vSplit :: Action ()
vSplit = V.windows %= V.vSplit
