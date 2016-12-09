module Rasa.Ext.Directive
  ( exit
  , insertTextAt
  , deleteCharAt
  , addBuffer
  ) where

import Rasa.Ext

import Control.Lens
import qualified Data.Text as T
import Data.Monoid

addBuffer :: T.Text -> Alteration ()
addBuffer txt = buffers %= (++[newBuffer txt])

exit :: Alteration ()
exit = do
  exiting .= True
  event .= [Exit]

insertTextAt :: Int -> T.Text -> T.Text -> T.Text
insertTextAt i new txt = T.take i txt <> new <> T.drop i txt

deleteCharAt :: Int -> T.Text -> T.Text
deleteCharAt i txt = T.take i txt <> T.drop (i + 1) txt
