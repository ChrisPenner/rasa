module Rasa.Ext.Views.Internal.Actions
  ( rotate
  , closeInactive
  , focusViewLeft
  , focusViewRight
  , focusViewAbove
  , focusViewBelow
  , hSplit
  , vSplit
  , addSplit
  , nextBuf
  , prevBuf
  , focusDo
  , focusDo_
  , focusedBufs
  , isFocused
  , scrollBy
  ) where

import Rasa.Ext
import qualified Rasa.Ext.Views.Internal.Views as V
import Rasa.Ext.Views.Internal.BiTree

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.List

-- | Flip all Horizontal splits to Vertical ones and vice versa.
rotate :: App ()
rotate = V.overWindows V.rotate

-- | Move focus from any viewports one viewport to the left
focusViewLeft :: App ()
focusViewLeft = V.overWindows V.focusViewLeft

-- | Move focus from any viewports one viewport to the right
focusViewRight :: App ()
focusViewRight = V.overWindows V.focusViewRight

-- | Move focus from any viewports one viewport above
focusViewAbove :: App ()
focusViewAbove = V.overWindows V.focusViewAbove

-- | Move focus from any viewports one viewport below
focusViewBelow :: App ()
focusViewBelow = V.overWindows V.focusViewBelow

-- | Close all inactive viewports
closeInactive :: App ()
closeInactive = do
  mWindows <- V.getViews
  V.setViews $ mWindows >>= V.closeBy (not . view V.active)

-- | Split active views horizontally
hSplit :: App ()
hSplit = V.overWindows V.hSplit

-- | Split active views vertically
vSplit :: App ()
vSplit = V.overWindows V.vSplit

-- | Add a new split at the top level in the given direction containing the given buffer.
addSplit :: BufAdded -> App ()
addSplit (BufAdded bRef) = do
  mWin <- V.getViews
  case mWin of
    Nothing -> V.setViews . Just $ Leaf (V.View True (V.BufView bRef) 0)
    Just win -> V.setViews . Just $ V.addSplit V.Vert (V.BufView bRef) win

-- | Select the next buffer in any active viewports
nextBuf :: App ()
nextBuf = V.traverseViews next
  where
    next vw
      | vw ^. V.active = do
        newViewable <- getNextBufRef (vw^. V.viewable)
        return (vw &  V.viewable .~ newViewable)
      | otherwise = return vw

    getNextBufRef (V.BufView br) = V.BufView <$> nextBufRef br
    getNextBufRef v = return v

-- | Select the previous buffer in any active viewports
prevBuf :: App ()
prevBuf = V.traverseViews prev
  where
    prev vw
      | vw ^. V.active = do
        newViewable <- getPrevBufRef (vw^. V.viewable)
        return (vw &  V.viewable .~ newViewable)
      | otherwise = return vw

    getPrevBufRef (V.BufView br) = V.BufView <$> prevBufRef br
    getPrevBufRef v = return v

-- | Get bufRefs for all buffers that are selected in at least one viewport
focusedBufs :: App [BufRef]
focusedBufs = do
  mWindows <- V.getViews
  case mWindows of
    Nothing -> return []
    Just win -> return . nub . activeBufRefs $ win
  where activeBufRefs = toListOf $ traverse . filtered (view V.active) . V.viewable . V._BufViewRef

-- | Returns whether the current buffer is focused in at least one view.
isFocused :: BufAction Bool
isFocused = do
  inFocus <- liftAction focusedBufs
  br <- getBufRef
  return $ br `elem` inFocus

-- | Run a bufAction over all focused buffers and return any results.
focusDo :: BufAction a -> App [a]
focusDo bufAct = do
  bufRefs <- focusedBufs
  catMaybes <$> mapM (`bufDo` bufAct) bufRefs

-- | 'focusDo' with a void return
focusDo_ :: BufAction a -> App ()
focusDo_ = void . focusDo

-- | Scrolls each focused viewport by the given amount.
scrollBy :: Int -> App ()
scrollBy amt = V.overWindows $ V.scrollBy amt
