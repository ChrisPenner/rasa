module Rasa.Ext.Directive
  ( exit
  , insertTextAt
  , deleteCharAt
  , addBuffer
  , addBufferThen
  , bufDo
  , focusDo
  ) where

import Rasa.Ext
import Rasa.State
import Rasa.Alteration
import Rasa.Buffer
import Control.Monad.IO.Class

import Control.Lens
import qualified Data.Text as T
import Data.Monoid

bufDo :: BufAction () -> Alteration ()
bufDo = Alteration . zoom (buffers . traverse) . getBufAction

focusDo :: BufAction () -> Alteration ()
focusDo = Alteration . zoom focusedBuf . getBufAction

addBuffer :: T.Text -> Alteration ()
addBuffer txt = buffers %= (++[newBuffer txt])

addBufferThen :: T.Text -> BufAction a -> Alteration a
addBufferThen txt act = do
  (a, newBuf) <- liftIO $ runBufAction (newBuffer txt) act
  buffers %= (++[newBuf])
  return a

exit :: Alteration ()
exit = do
  exiting .= True
  event .= [Exit]

insertTextAt :: Int -> T.Text -> T.Text -> T.Text
insertTextAt i new txt = T.take i txt <> new <> T.drop i txt

deleteCharAt :: Int -> T.Text -> T.Text
deleteCharAt i txt = T.take i txt <> T.drop (i + 1) txt

