module Rasa.Ext.Slate (slate) where

import Rasa.Ext
import Rasa.Ext.Slate.Internal.Render
import Rasa.Ext.Slate.Internal.Event
import Rasa.Ext.Slate.Internal.State

import qualified Graphics.Vty as V
import Control.Monad.IO.Class

-- | The main export for this extension. Add this to your user config.
--
-- e.g.
--
-- > rasa $ do
-- >    slate
-- >    ...
slate :: App ()
slate = do
  onInit terminalEvents
  onEveryRender_ renderAll
  onExit shutdown

-- | Call vty shutdown procedure (if this doesn't happen the terminal ends up in strange states)
shutdown :: App ()
shutdown = do
  v <- getVty
  liftIO $ V.shutdown v
