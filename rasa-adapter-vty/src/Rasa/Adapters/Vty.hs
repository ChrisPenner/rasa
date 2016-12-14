module Rasa.Adapters.Vty (vty, vtyEvent) where

import Rasa.Ext
import Rasa.Adapters.Vty.Render (render')
import Rasa.Adapters.Vty.Event (vtyEvent)
import Rasa.Adapters.Vty.State (getVty)

import qualified Graphics.Vty as V
import Control.Monad.IO.Class
import Control.Lens

vty :: Alteration ()
vty = do
  evt <- use event
  if Exit `elem` evt then shutdown
                     else render'

shutdown :: Alteration ()
shutdown = do
  v <- getVty
  liftIO $ V.shutdown v
