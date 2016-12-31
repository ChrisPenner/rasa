module Rasa.Renderer.Slate (slate) where

import Rasa.Ext
import Rasa.Renderer.Slate.Render (render)
import Rasa.Renderer.Slate.Event (terminalEvents)
import Rasa.Renderer.Slate.State (getVty)

import qualified Graphics.Vty as V
import Control.Monad.IO.Class

-- | The main export for this extension. Add this to your user config.
--
-- e.g.
--
-- > rasa [...] $ do
-- >    slate
-- >    ...
slate :: Scheduler ()
slate = do
  onInit terminalEvents
  onRender render
  onExit shutdown

-- | Call vty shutdown procedure (if this doesn't happen the terminal ends up in strange states)
shutdown :: Action ()
shutdown = do
  v <- getVty
  liftIO $ V.shutdown v
