{-# language OverloadedStrings #-}
module Rasa.Internal.BufActions
  ( overRange
  , replaceRange
  , deleteRange
  , insertAt
  , sizeOf
  , getLineRange

  -- * Performing Apps on Buffers
  , bufDo
  , bufDo_
  , buffersDo
  , buffersDo_

  -- * Editor Apps
  , getBufRefs
  , nextBufRef
  , prevBufRef

  , getBufExt
  , setBufExt
  , overBufExt

  , getBufRef
  , getRange
  , addBuffer
  , getBuffer


  , dispatchBufEvent
  , addBufListener
  , addBufListener_
  , removeBufListener

  , onBufAdded
  , onBufAdded_
  , dispatchBufAdded
  , onEveryNewBuffer
  , onEveryNewBuffer_

  , onBufTextChanged
  , dispatchBufTextChanged

  , getText

  ) where

import Reflex

import Rasa.Internal.Buffer
import Rasa.Internal.Range
import Rasa.Internal.Text
import Rasa.Internal.Events
import Rasa.Internal.Listeners

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Default
import Data.Typeable
import qualified Yi.Rope as Y
import qualified Data.IntMap as IM

-- | Returns the text of the current buffer
getText :: BufAction Y.YiString
getText = use text

-- -- | Sets the text of the current buffer
-- setText :: Y.YiString -> BufAction ()
-- setText txt = text .= txt

-- | Gets the range of text from the buffer
getRange :: CrdRange -> BufAction Y.YiString
getRange rng = view (range rng) <$> getText

-- | Sets the range of text from the buffer
setRange :: CrdRange -> Y.YiString -> BufAction ()
setRange rng txt = do
  text.range rng .= txt
  dispatchBufTextChanged $ BufTextChanged rng txt

-- | Gets the current buffer's 'BufRef'
getBufRef :: BufAction BufRef
getBufRef = use ref

-- | Retrieve some buffer extension state
getBufExt :: (Typeable ext, Show ext, Default ext) => BufAction ext
getBufExt = use ext

-- | Set some buffer extension state
setBufExt :: (Typeable ext, Show ext, Default ext) => ext -> BufAction ()
setBufExt newExt = ext .= newExt

-- | Set some buffer extension state
overBufExt :: (Typeable ext, Show ext, Default ext) => (ext -> ext) -> BufAction ()
overBufExt f = ext %= f

-- -- | This lifts up an 'Action' to be run inside a 'BufAction'
-- liftAction :: App r -> BufAction r
-- liftAction action = liftBufAction $ LiftAction action id

-- | Runs function over given range of text
overRange :: CrdRange -> (Y.YiString -> Y.YiString) -> BufAction ()
overRange r f = getRange r >>= setRange r . f

-- | Deletes the text in the given range from the buffer.
deleteRange :: CrdRange -> BufAction ()
deleteRange r = replaceRange r ""

-- | Replaces the text in the given range with the given text.
replaceRange :: CrdRange -> Y.YiString -> BufAction ()
replaceRange r txt = overRange r (const txt)

-- | Inserts text into the buffer at the given 'Coord'.
insertAt :: Coord -> Y.YiString -> BufAction ()
insertAt c = replaceRange r
  where r = Range c c

-- | Rows can be represented by their line number.
type Row = Int

-- | Gets the range representing a given row (if that row exists)
getLineRange :: Row -> BufAction (Maybe CrdRange)
getLineRange n = do
  txt <- getText
  let len = txt ^? asLines . ix n . to Y.length
  return $ Range (Coord n 0) . Coord n <$> len

-- | Adds a new buffer and returns the BufRef
addBuffer :: Y.YiString -> App BufRef
addBuffer txt = do
  bufId <- nextBufId <+= 1
  let bufRef = BufRef bufId
  buffers.at bufId ?= mkBuffer txt bufRef
  dispatchBufAdded (BufAdded bufRef)
  return bufRef

-- | Returns an up-to-date list of all 'BufRef's
getBufRefs :: App [BufRef]
getBufRefs = fmap BufRef <$> use (buffers.to IM.keys)


-- | Retrieve a buffer. This is read-only for logging/rendering/debugging purposes only.
getBuffer :: BufRef -> App (Maybe Buffer)
getBuffer (BufRef bufInd) =
  use (buffers.at bufInd)

-- | Runs a BufAction over the given BufRefs, returning any results.
--
-- Result list is not guaranteed to be the same length or positioning as input BufRef list; some buffers may no
-- longer exist.
-- TODO!!!
bufferDo :: [BufRef] -> BufAction r -> App [r]
bufferDo bufRefs bufAct = do
  results <- forM bufRefs $ \(BufRef bInd) -> do
    zoomer (buffers.at bInd._Just) ((:[]) <$> bufAct)
  return . concat $ results

zoomer l act = do
  s <- get
  (r, s) <- get >>= runStateT (zoom l act)
  put s
  return r

-- | This lifts a 'Rasa.App.BufAction' to an 'Rasa.App.App' which
-- performs the 'Rasa.App.BufAction' on every buffer and collects the return
-- values as a list.

buffersDo :: BufAction a -> App [a]
buffersDo bufAct = do
  bufRefs <- getBufRefs
  bufferDo bufRefs bufAct

buffersDo_ :: BufAction a -> App ()
buffersDo_ = void . buffersDo

-- | This lifts a 'Rasa.Internal.App.BufAction' to an 'Rasa.Internal.App.App' which
-- performs the 'Rasa.Internal.App.BufAction' on the buffer referred to by the 'BufRef'
-- If the buffer referred to no longer exists this returns: @Nothing@.
bufDo :: BufRef -> BufAction a -> App (Maybe a)
bufDo bufRef bufAct = listToMaybe <$> bufferDo [bufRef] bufAct

bufDo_ :: BufRef -> BufAction a -> App ()
bufDo_ bufRef bufAct = void $ bufDo bufRef bufAct

-- | Gets 'BufRef' that comes after the one provided
nextBufRef :: BufRef -> App BufRef
nextBufRef br = do
  bufRefs <- getBufRefs
  return $ if null bufRefs
              then br
              else case dropWhile (<= br) bufRefs of
                     [] -> head bufRefs
                     (x:_) -> x

-- | Gets 'BufRef' that comes before the one provided
prevBufRef :: BufRef -> App BufRef
prevBufRef br = do
  bufRefs <- getBufRefs
  return $ if null bufRefs
              then br
              else case dropWhile (>= br) (reverse bufRefs) of
                     [] -> last bufRefs
                     (x:_) -> x



-- | Registers an action to be performed after a new buffer is added.
--
-- The supplied function will be called with a 'BufRef' to the new buffer, and the resulting 'App' will be run.
onBufAdded :: (BufAdded -> App result) -> App ListenerId
onBufAdded actionF = addListener (void . actionF)

onBufAdded_ :: (BufAdded -> App result) -> App ()
onBufAdded_ = void . onBufAdded

-- | Run the given 'BufAction' over all new buffers
onEveryNewBuffer :: BufAction a -> App ListenerId
onEveryNewBuffer bufApp = onBufAdded $
  \(BufAdded br) -> bufDo_ br bufApp

onEveryNewBuffer_ :: BufAction a -> App ()
onEveryNewBuffer_ = void . onEveryNewBuffer

-- | Dispatch the 'BufAdded' action.
dispatchBufAdded :: BufAdded -> App ()
dispatchBufAdded = dispatchEvent

-- | This is fired every time text in a buffer changes.
--
-- The range of text which was altered and the new value of that text are provided inside a 'BufTextChanged' event.
onBufTextChanged :: (BufTextChanged -> BufAction result) -> BufAction ListenerId
onBufTextChanged bufAppF = addBufListener (void . bufAppF)

-- | Dispatch the 'BufBufTextChanged' action.
dispatchBufTextChanged :: BufTextChanged -> BufAction ()
dispatchBufTextChanged = dispatchBufEvent

-- | Dispatches an event of any type to the BufAction's buffer.
-- See 'dispatchEvent'
dispatchBufEvent :: (Monoid result, Typeable eventType, Typeable result) => (eventType -> BufAction result)
dispatchBufEvent = dispatchEvent

-- | Adds a listener to the BufAction's buffer.
-- See 'addListener'
addBufListener :: (Typeable eventType, Typeable result, Monoid result) => (eventType -> BufAction result) -> BufAction ListenerId
addBufListener = addListener

addBufListener_ :: (Typeable eventType, Typeable result, Monoid result) => (eventType -> BufAction result) -> BufAction ()
addBufListener_ = void . addBufListener

-- | Removes a listener from the BufAction's buffer.
-- See 'removeListener'
removeBufListener :: ListenerId -> BufAction ()
removeBufListener = removeListener
