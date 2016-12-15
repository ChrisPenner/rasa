module Rasa.Renderer.Slate (slate, slateEvent) where

import Rasa.Ext
import Rasa.Ext.Scheduler
import Rasa.Renderer.Slate.Render (render)
import Rasa.Renderer.Slate.Event (slateEvent)
import Rasa.Renderer.Slate.State (getVty)

import qualified Graphics.Vty as V
import Control.Monad.IO.Class

slate :: Scheduler ()
slate = do
  onRender render
  onExit shutdown

shutdown :: Action ()
shutdown = do
  v <- getVty
  liftIO $ V.shutdown v
