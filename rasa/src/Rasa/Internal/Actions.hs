{-# language Rank2Types, OverloadedStrings #-}
module Rasa.Internal.Actions
  (
  -- * Performing Actions on Buffers
    bufDo
  , bufDo_
  , buffersDo
  , buffersDo_

  -- * Editor Actions
  , exit
  , newBuffer
  , getBufRefs
  -- , getBuffers
  -- , getBuffer
  , nextBufRef
  , prevBufRef
  ) where

import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.BufAction
import Rasa.Internal.Events

import Control.Monad
import Data.Maybe
import qualified Yi.Rope as Y


-- | This lifts a 'Rasa.Action.BufAction' to an 'Rasa.Action.Action' which
-- performs the 'Rasa.Action.BufAction' on every buffer and collects the return
-- values as a list.

buffersDo :: BufAction a -> Action [a]
buffersDo bufAct = do
  bufRefs <- getBufRefs
  bufferDo bufRefs bufAct 

buffersDo_ :: BufAction a -> Action ()
buffersDo_ = void . buffersDo

-- | This lifts a 'Rasa.Internal.Action.BufAction' to an 'Rasa.Internal.Action.Action' which
-- performs the 'Rasa.Internal.Action.BufAction' on the buffer referred to by the 'BufRef'
-- If the buffer referred to no longer exists this returns: @Nothing@.
bufDo :: BufRef -> BufAction a -> Action (Maybe a)
bufDo bufRef bufAct = listToMaybe <$> bufferDo [bufRef] bufAct

bufDo_ :: BufRef -> BufAction a -> Action ()
bufDo_ bufRef bufAct = void $ bufDo bufRef bufAct

-- | This adds a new buffer with the given text, returning a reference to that buffer.
newBuffer :: Y.YiString -> Action BufRef
newBuffer txt = do
  bufRef <- addBuffer
  bufferDo [bufRef] (setText txt)
  dispatchEvent (BufAdded bufRef)
  return bufRef

-- | Returns the 'Buffer' for a BufRef if it still exists.
-- This is read-only; altering the buffer has no effect on the stored buffer.
-- This function is useful for renderers.
-- getBuffer :: BufRef -> Action (Maybe Buffer)
-- getBuffer (BufRef bufInt) = use (buffers.at bufInt)

-- | Returns an up-to-date list of all 'Buffer's, returned values
-- are read-only; altering them has no effect on the actual stored buffers.
-- This function is useful for renderers.
-- getBuffers :: Action [(BufRef, Buffer)]
-- getBuffers = fmap (first BufRef) <$> use (buffers.to assocs)

-- | Gets 'BufRef' that comes after the one provided
nextBufRef :: BufRef -> Action BufRef
nextBufRef br = do
  bufRefs <- getBufRefs
  return $ if null bufRefs
              then br
              else fromMaybe (head bufRefs) $ listToMaybe $ filter (>br) bufRefs

-- | Gets 'BufRef' that comes before the one provided
prevBufRef :: BufRef -> Action BufRef
prevBufRef br = do
  bufRefs <- getBufRefs
  return $ if null bufRefs
              then br
              else fromMaybe (last bufRefs) $ listToMaybe $ filter (<br) bufRefs

