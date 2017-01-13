module Rasa.Ext.Views.Internal.Actions where

import Rasa.Ext
import qualified Rasa.Ext.Views.Internal.Views as V
import Rasa.Ext.Views.Internal.BiTree

import Control.Lens
import Data.Maybe
import Data.List

-- | Main export from the views extension, add this to your rasa config.
views :: Scheduler ()
views = onBufAdded addSplit

-- | Flip all Horizontal splits to Vertical ones and vice versa.
rotate :: Action ()
rotate = V.windows._Just %= V.rotate

-- | Move focus from any viewports one viewport to the left
focusViewLeft :: Action ()
focusViewLeft = V.windows._Just %= V.focusViewLeft

-- | Move focus from any viewports one viewport to the right
focusViewRight :: Action ()
focusViewRight = V.windows._Just %= V.focusViewRight

-- | Move focus from any viewports one viewport above
focusViewAbove :: Action ()
focusViewAbove = V.windows._Just %= V.focusViewAbove

-- | Move focus from any viewports one viewport below
focusViewBelow :: Action ()
focusViewBelow = V.windows._Just %= V.focusViewBelow

-- | Close all inactive viewports
closeInactive :: Action ()
closeInactive = V.windows %= (>>= V.closeBy (not . view V.active))

-- | Split active views horizontally
hSplit :: Action ()
hSplit = V.windows._Just %= V.hSplit

-- | Split active views vertically
vSplit :: Action ()
vSplit = V.windows._Just %= V.vSplit

-- | Add a new split at the top level in the given direction containing the given buffer.
addSplit :: BufRef -> Action ()
addSplit bRef = do
  mWin <- use V.windows
  case mWin of
    Nothing -> V.windows ?= Leaf (V.View True bRef)
    Just win -> V.windows ?= V.addSplit V.Vert bRef win

-- | Select the next buffer in any active viewports
nextBuf :: Action ()
nextBuf = do
  mWin <- use V.windows
  case mWin of
    Nothing -> return ()
    Just win -> V.windows._Just <~ traverse next win
  where
    next vw =
      if vw ^. V.active
          then do
            newBufRef <- nextBufRef (vw ^. V.bufRef)
            return (vw & V.bufRef .~ newBufRef)
          else return vw

-- | Select the previous buffer in any active viewports
prevBuf :: Action ()
prevBuf = do
  mWin <- use V.windows
  case mWin of
    Nothing -> return ()
    Just win -> V.windows._Just <~ traverse prev win
  where
    prev vw =
      if vw ^. V.active
         then do
           newBufRef <- prevBufRef (vw ^. V.bufRef)
           return (vw & V.bufRef .~ newBufRef)
         else return vw

-- | Get bufRefs for all buffers that are selected in at least one viewport
focusedBufs :: Action [BufRef]
focusedBufs = use $ V.windows._Just.to activeBufRefs.to nub
  where activeBufRefs = toListOf $ traverse . filtered (view V.active) . V.bufRef

-- | Run a bufAction over all focused buffers and return any results.
focusDo :: BufAction a -> Action [a]
focusDo bufAct = do
  bufRefs <- focusedBufs
  catMaybes <$> mapM (`bufDo` bufAct) bufRefs
