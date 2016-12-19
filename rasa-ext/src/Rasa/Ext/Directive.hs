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
  , insertTextAt
  , deleteCharAt
  ) where

import Rasa.Ext
import Rasa.State
import Rasa.Action
import Rasa.Buffer as B
import Control.Monad.IO.Class

import Control.Lens
import qualified Yi.Rope as Y
import qualified Data.Text as T
import Data.Monoid

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
addBuffer txt = buffers %= (++[newBuffer txt])

-- | This adds a new buffer with the given text then performs the given
-- 'Rasa.Action.BufAction' agains that buffer.
addBufferThen :: T.Text -> BufAction a -> Action a
addBufferThen txt act = do
  (a, newBuf) <- liftIO $ runBufAction (newBuffer txt) act
  buffers %= (++[newBuf])
  return a

-- | This signals to the editor that you'd like to shutdown. The current events
-- will finish processing, then the 'Rasa.Ext.Scheduler.onExit' hook will run,
-- then the editor will exit.

exit :: Action ()
exit = exiting .= True

-- | Inserts text at the specified index into the buffer's text.
insertTextAt :: Int -> T.Text -> BufAction ()
insertTextAt o new =
  let insertTxt txt = Y.take o txt <> Y.fromText new <> Y.drop o txt
   in rope %= insertTxt

-- | Inserts text at the specified index into the buffer's text.
deleteCharAt :: Int -> BufAction ()
deleteCharAt o = B.rope %= deleteChar
  where deleteChar txt = Y.take o txt <> Y.drop (o + 1) txt

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
