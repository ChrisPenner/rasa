module Rasa.Ext.Views.Internal.Actions
  ( rotate
  , closeInactive
  , focusViewLeft
  , focusViewRight
  , focusViewAbove
  , focusViewBelow
  , addRenderableSplit
  , autoAddBufSplit
  , nextBuf
  , prevBuf
  , focusDo
  , focusDo_
  , activeViewsDo
  , activeViewsDo_
  , viewBufDo
  , viewBufDo_
  , focusedBufs
  , isFocused
  , hSplit
  , vSplit
  , onNewView
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
rotate = V.viewTree._Just %= V.rotate

-- | Move focus from any viewports one viewport to the left
focusViewLeft :: App ()
focusViewLeft = V.viewTree._Just %= V.focusViewLeft

-- | Move focus from any viewports one viewport to the right
focusViewRight :: App ()
focusViewRight = V.viewTree._Just %= V.focusViewRight

-- | Move focus from any viewports one viewport above
focusViewAbove :: App ()
focusViewAbove = V.viewTree._Just %= V.focusViewAbove

-- | Move focus from any viewports one viewport below
focusViewBelow :: App ()
focusViewBelow = V.viewTree._Just %= V.focusViewBelow

-- | Close all inactive viewports
closeInactive :: App ()
closeInactive = V.viewTree %= close
  where
    close tree = tree >>= V.closeBy (not . view V.active)

-- | Add a new split at the top level containing the given Renderable
addRenderableSplit :: Renderable r => r -> App ()
addRenderableSplit r = do
  mWin <- use V.viewTree
  case mWin of
    Nothing -> do
      vw <- V.mkView False (V.VRenderable r)
      V.viewTree ?= Leaf vw
    Just win -> do
      vw <- V.mkView False (V.VRenderable r)
      V.viewTree ?= V.addSplit V.Vert vw win

-- | Add a new split at the top level in the given direction containing the given buffer.
autoAddBufSplit :: BufAdded -> App ()
autoAddBufSplit (BufAdded bRef) = do
  mWin <- use V.viewTree
  case mWin of
    Nothing -> do
      vw <- (V.active .~ True) <$> V.mkBufView bRef
      V.viewTree ?= Leaf vw
    Just win -> do
      vw <- V.mkBufView bRef
      V.viewTree ?= V.addSplit V.Vert vw win

-- | Select the next buffer in any active viewports
nextBuf :: App ()
nextBuf = activeViewsDo_ next
  where
    next = do
      mBufRef <- preuse (V.viewable . V._BufViewRef)
      maybe (return ()) getNext mBufRef
    getNext bRef = do
      nextRef <- runApp $ nextBufRef bRef
      V.viewable . V._BufViewRef .= nextRef

-- | Select the previous buffer in any active viewports
prevBuf :: App ()
prevBuf = activeViewsDo_ prev
  where
    prev = do
      mBufRef <- preuse (V.viewable . V._BufViewRef)
      maybe (return ()) getPrev mBufRef
    getPrev bRef = do
        prevRef <- runApp $ prevBufRef bRef
        V.viewable . V._BufViewRef .= prevRef

-- | Get bufRefs for all buffers that are selected in at least one viewport
focusedBufs :: App [BufRef]
focusedBufs = uses (V.viewTree . _Just) (nub . activeBufRefs)
    where activeBufRefs = toListOf $ traverse . filtered (view V.active) . V.viewable . V._BufViewRef

-- | Returns whether the current buffer is focused in at least one view.
isFocused :: BufAction Bool
isFocused = do
  inFocus <- runApp focusedBufs
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

viewBufDo :: Monoid a => BufAction a -> V.ViewAction a
viewBufDo bufAction = do
  mBufRef <- preuse (V.viewable . V._BufViewRef)
  fromMaybe mempty <$> case mBufRef of
                       Just bufRef -> runApp $ bufDo bufRef bufAction
                       Nothing -> return $ Just mempty

viewBufDo_ :: Monoid a => BufAction a -> V.ViewAction ()
viewBufDo_ = void . viewBufDo

activeViewsDo :: Monoid a => V.ViewAction a -> App a
activeViewsDo = runActionOver (V.viewTree._Just.traverse.filtered (view V.active))

activeViewsDo_ :: Monoid a => V.ViewAction a -> App ()
activeViewsDo_ = void . activeViewsDo

-- | Split active views horizontally
hSplit :: App ()
hSplit = V.viewTree._Just %= V.splitView V.Hor

-- | Split active views vertically
vSplit :: App ()
vSplit = V.viewTree._Just %= V.splitView V.Vert

onNewView :: V.ViewAction () -> App ()
onNewView action = V.newViewListeners <>= action
