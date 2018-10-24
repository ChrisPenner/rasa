module Rasa.Ext.Logger
  ( logger
  , logInfo
  , logError
  ) where

import Rasa.Ext

import Control.Monad.State
import Control.Monad.IO.Class (MonadIO)

logger :: App ()
logger = do
  liftIO $ writeFile "logs.log" "Event Log\n"
  onEveryRender_ $ return ()
    -- ed <- getEditor
    -- liftIO $ appendFile "logs.log" (show ed)

logInfo :: MonadIO m => String -> m ()
logInfo msg = liftIO $ appendFile "info.log" ("INFO: " ++ msg ++ "\n")

logError :: MonadIO m => String -> m ()
logError msg = liftIO $ appendFile "error.log" ("ERROR: " ++ msg ++ "\n")
