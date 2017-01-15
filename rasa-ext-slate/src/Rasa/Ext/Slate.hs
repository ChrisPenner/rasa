module Rasa.Ext.Slate (slate) where

import Rasa.Ext
import Rasa.Ext.Slate.Internal.Render (render)
import Rasa.Ext.Slate.Internal.Event (terminalEvents)
import Rasa.Ext.Slate.Internal.State (getVty)

import qualified Graphics.Vty as V
import Control.Monad.IO.Class

-- | The main export for this extension. Add this to your user config.
--
-- e.g.
--
-- > rasa $ do
-- >    slate
-- >    ...
slate :: Action ()
slate = do
  onInit terminalEvents
  onEveryRender_ render
  onExit shutdown

-- | Call vty shutdown procedure (if this doesn't happen the terminal ends up in strange states)
shutdown :: Action ()
shutdown = do
  v <- getVty
  liftIO $ V.shutdown v
