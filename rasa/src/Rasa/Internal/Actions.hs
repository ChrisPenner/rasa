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
  , getBuffers
  , getBuffer
  , nextBufRef
  , prevBufRef

  -- * Buffer Actions
  , overRange
  , replaceRange
  , deleteRange
  , insertAt
  , sizeOf
  ) where

import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.BufAction
import Rasa.Internal.Range
import Rasa.Internal.Listeners
import Rasa.Internal.Events
import Rasa.Internal.Buffer as B

import Control.Lens
import Control.Monad
import Control.Arrow (first)
import Data.Maybe
import Data.IntMap as M
import qualified Yi.Rope as Y


-- | This lifts a 'Rasa.Action.BufAction' to an 'Rasa.Action.Action' which
-- performs the 'Rasa.Action.BufAction' on every buffer and collects the return
-- values as a list.

buffersDo :: BufAction a -> Action [a]
buffersDo bufAct = do
  bufRefs <- getBufRefs
  catMaybes . foldMap (:[]) <$> mapM (runBufAction bufAct) bufRefs

buffersDo_ :: BufAction a -> Action ()
buffersDo_ = void . buffersDo

-- | This lifts a 'Rasa.Action.BufAction' to an 'Rasa.Action.Action' which
-- performs the 'Rasa.Action.BufAction' on the buffer referred to by the 'BufRef'
-- If the buffer referred to no longer exists this returns @Action Nothing@.
bufDo :: BufRef -> BufAction a -> Action (Maybe a)
bufDo bufRef bufAct = runBufAction bufAct bufRef

bufDo_ :: BufRef -> BufAction a -> Action ()
bufDo_ bufRef bufAct = void $ bufDo bufRef bufAct

-- | This adds a new buffer with the given text.
newBuffer :: Y.YiString -> Action BufRef
newBuffer txt = do
  n <- nextBufId <<+= 1
  buffers %= insert n (mkBuffer txt)
  let bufRef = BufRef n
  dispatchEvent (BufAdded bufRef)
  return bufRef

-- | Returns an up-to-date list of all 'BufRef's
getBufRefs :: Action [BufRef]
getBufRefs = fmap BufRef <$> use (buffers.to keys)

-- | Returns the 'Buffer' for a BufRef if it still exists.
-- This is read-only; altering the buffer has no effect on the stored buffer.
-- This function is useful for renderers.
getBuffer :: BufRef -> Action (Maybe Buffer)
getBuffer (BufRef bufInt) = use (buffers.at bufInt)

-- | Returns an up-to-date list of all 'Buffer's, returned values
-- are read-only; altering them has no effect on the actual stored buffers.
-- This function is useful for renderers.
getBuffers :: Action [(BufRef, Buffer)]
getBuffers = fmap (first BufRef) <$> use (buffers.to assocs)

-- | Gets 'BufRef' that comes after the one provided
nextBufRef :: BufRef -> Action BufRef
nextBufRef br@(BufRef bufInt) = do
  bufMap <- use buffers
  if M.null bufMap
     then return br
     else do
       let mGreaterInd = lookupGT bufInt bufMap
       case mGreaterInd of
         Just (greaterInd, _) -> return $ BufRef greaterInd
         Nothing -> return . BufRef . fst . findMin $ bufMap

-- | Gets 'BufRef' that comes before the one provided
prevBufRef :: BufRef -> Action BufRef
prevBufRef br@(BufRef bufInt) = do
  bufMap <- use buffers
  if M.null bufMap
     then return br
     else do
       let mLesserInd = lookupLT bufInt bufMap
       case mLesserInd of
         Just (lesserInd, _) -> return $ BufRef lesserInd
         Nothing -> return . BufRef . fst . findMax $ bufMap

-- | This signals to the editor that you'd like to shutdown. The current events
-- will finish processing, then the 'Rasa.Ext.Listeners.onExit' event will be dispatched,
-- then the editor will exit.

exit :: Action ()
exit = exiting .= True

-- | Deletes the text in the given range from the buffer.
deleteRange :: CrdRange -> BufAction ()
deleteRange r = replaceRange r ""

-- | Replaces the text in the given range from the buffer.
replaceRange :: CrdRange -> Y.YiString -> BufAction ()
replaceRange r txt = overRange r (const txt)

-- | Inserts text into the buffer at the given Coord.
insertAt :: Coord -> Y.YiString -> BufAction ()
insertAt c = replaceRange r
  where r = Range c c
