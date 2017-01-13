module Rasa.Ext.Views.Internal.Actions where

import Rasa.Ext
import qualified Rasa.Ext.Views.Internal.Views as V
import Rasa.Ext.Views.Internal.BiTree

import Control.Lens

views :: Scheduler ()
views = onBufAdded addSplit

refocusView :: Action ()
refocusView = V.windows._Just %= V.refocusView

focusViewLeft :: Action ()
focusViewLeft = V.windows._Just %= V.focusViewLeft

focusViewRight :: Action ()
focusViewRight = V.windows._Just %= V.focusViewRight

focusViewAbove :: Action ()
focusViewAbove = V.windows._Just %= V.focusViewAbove

focusViewBelow :: Action ()
focusViewBelow = V.windows._Just %= V.focusViewBelow

closeInactive :: Action ()
closeInactive = V.windows %= (>>= V.closeBy (not . view V.active))

hSplit :: Action ()
hSplit = V.windows._Just %= V.hSplit

vSplit :: Action ()
vSplit = V.windows._Just %= V.vSplit

addSplit :: BufRef -> Action ()
addSplit bRef = do
  mWin <- use V.windows
  case mWin of
    Nothing -> V.windows ?= Leaf (V.View True bRef)
    Just win -> V.windows ?= V.addSplit V.Vert bRef win
