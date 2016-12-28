{-# language Rank2Types, OverloadedStrings #-}
module Rasa.Internal.Directive
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
  , sizeOf
  ) where

import Rasa.Internal.Text
import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.Range
import Rasa.Internal.Buffer as B

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
  buffers <>= [newBuffer txt]
  fmap (!! 0) . Action . zoom (buffers._last) . fmap (:[]) . getBufAction $ act

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

-- | Deletes the text in the given range from the buffer.
deleteRange :: Range -> BufAction ()
deleteRange r = rope.range r.asText .= ""

-- | Replaces the text in the given range from the buffer.
replaceRange :: Range -> T.Text -> BufAction ()
replaceRange r txt = rope.range r.asText .= txt

-- | Inserts text into the buffer at the given Coord.
insertAt :: Coord -> T.Text -> BufAction ()
insertAt c txt = rope.range (Range c c).asText .= txt

-- | Runs the given function over the text in the range, replacing it with the results.
overRange :: Range -> (T.Text -> T.Text) -> BufAction ()
overRange r f = rope.range r.asText %= f

