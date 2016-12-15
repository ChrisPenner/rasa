module Rasa.Ext.Directive
  ( exit
  , insertTextAt
  , deleteCharAt
  , addBuffer
  , addBufferThen
  , bufDo
  , focusDo
  , nextBuf
  , prevBuf
  ) where

import Rasa.Ext
import Rasa.State
import Rasa.Action
import Rasa.Buffer
import Control.Monad.IO.Class

import Control.Lens
import qualified Data.Text as T
import Data.Monoid

bufDo :: Monoid a => BufAction a -> Action a
bufDo = Action . zoom (buffers . traverse) . getBufAction

focusDo :: BufAction a -> Action a
focusDo = Action . zoom focusedBuf . getBufAction

addBuffer :: T.Text -> Action ()
addBuffer txt = buffers %= (++[newBuffer txt])

addBufferThen :: T.Text -> BufAction a -> Action a
addBufferThen txt act = do
  (a, newBuf) <- liftIO $ runBufAction (newBuffer txt) act
  buffers %= (++[newBuf])
  return a

exit :: Action ()
exit = exiting .= True

insertTextAt :: Int -> T.Text -> T.Text -> T.Text
insertTextAt i new txt = T.take i txt <> new <> T.drop i txt

deleteCharAt :: Int -> T.Text -> T.Text
deleteCharAt i txt = T.take i txt <> T.drop (i + 1) txt

nextBuf :: Action ()
nextBuf = do
  numBuffers <- use (buffers.to length)
  focused %= (`mod` numBuffers) . (+1)

prevBuf :: Action ()
prevBuf = do
  numBuffers <- use (buffers.to length)
  focused %= (`mod` numBuffers) . subtract 1
