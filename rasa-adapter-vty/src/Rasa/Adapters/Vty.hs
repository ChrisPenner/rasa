module Rasa.Adapters.Vty (vty, vtyEvent) where

import Rasa.Ext
import Rasa.Ext.Scheduler
import Rasa.Adapters.Vty.Render (render)
import Rasa.Adapters.Vty.Event (vtyEvent)
import Rasa.Adapters.Vty.State (getVty)

import qualified Graphics.Vty as V
import Control.Monad.IO.Class

vty :: Scheduler ()
vty = do
  onRender render
  onExit shutdown

shutdown :: Alteration ()
shutdown = do
  v <- getVty
  liftIO $ V.shutdown v
