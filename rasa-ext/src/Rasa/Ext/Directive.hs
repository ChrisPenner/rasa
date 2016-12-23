{-# language Rank2Types, OverloadedStrings #-}
module Rasa.Ext.Directive
  (
  -- * Performing Actions on Buffers
    bufDo
  , focusDo

  -- * Editor Actions
  , exit
  , addBuffer
  , addBufferThen
  , nextBuf
  , prevBuf

  -- * Buffer Actions
  , overRange
  , replaceRange
  , deleteRange
  , insertAt
  , rangeSize
  -- , toCoord
  -- , toOffset
  ) where

import Rasa.Text
import Rasa.State
import Rasa.Action
import Rasa.Range
import Rasa.Buffer as B
import Control.Monad.IO.Class

import Control.Lens
import qualified Data.Text as T

-- | This lifts a 'Rasa.Action.BufAction' to an 'Rasa.Action.Action' which
-- performs the 'Rasa.Action.BufAction' on the focused buffer.

focusDo :: BufAction a -> Action a
focusDo = Action . zoom focusedBuf . getBufAction


-- | This lifts a 'Rasa.Action.BufAction' to an 'Rasa.Action.Action' which
-- performs the 'Rasa.Action.BufAction' on every buffer and collects the return
-- values via 'mappend'

bufDo :: Monoid a => BufAction a -> Action a
bufDo = Action . zoom (buffers . traverse) . getBufAction

-- | This adds a new buffer with the given text.

addBuffer :: T.Text -> Action ()
addBuffer txt = buffers <>= [newBuffer txt]

-- | This adds a new buffer with the given text then performs the given
-- 'Rasa.Action.BufAction' agains that buffer.
addBufferThen :: T.Text -> BufAction a -> Action a
addBufferThen txt act = do
  (a, newBuf) <- liftIO $ runBufAction (newBuffer txt) act
  buffers <>= [newBuf]
  return a

-- | This signals to the editor that you'd like to shutdown. The current events
-- will finish processing, then the 'Rasa.Ext.Scheduler.onExit' hook will run,
-- then the editor will exit.

exit :: Action ()
exit = exiting .= True

-- | Switches focus to the next buffer
nextBuf :: Action ()
nextBuf = do
  numBuffers <- use (buffers.to length)
  focused %= (`mod` numBuffers) . (+1)

-- | Switches focus to the previous buffer
prevBuf :: Action ()
prevBuf = do
  numBuffers <- use (buffers.to length)
  focused %= (`mod` numBuffers) . subtract 1


toCoord :: Offset -> BufAction Coord
toCoord o = do
  txt <- use rope
  return $ o^.asCoord txt

toOffset :: Coord -> BufAction Offset
toOffset c = do
  txt <- use rope
  return $ c^.from (asCoord txt)

deleteRange :: Range -> BufAction ()
deleteRange r = rope.range r.asText .= ""

replaceRange :: Range -> T.Text -> BufAction ()
replaceRange r txt = rope.range r.asText .= txt

insertAt :: Cursor -> T.Text -> BufAction ()
insertAt c txt = rope.range (Range c c).asText .= txt

overRange :: Range -> (T.Text -> T.Text) -> BufAction ()
overRange r f = rope.range r.asText %= f

rangeSize :: Range -> BufAction Int
rangeSize r = do
  txt <- use rope
  let (Offset s, Offset e) = asOffsets txt r
  return (e - s)
