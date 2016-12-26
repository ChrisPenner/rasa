module Rasa.Ext.Logger
  ( logger
  , logInfo
  , logError
  ) where

import Rasa.Ext
import Rasa.Ext.Scheduler

import Control.Monad.State

logger :: Scheduler ()
logger = do
  onInit $ liftIO $ writeFile "logs.log" "Event Log\n"
  afterRender $ do
    editor <- get
    liftIO $ appendFile "logs.log" (show editor)

logInfo :: String -> Action ()
logInfo msg = liftIO $ appendFile "info.log" ("INFO: " ++ msg ++ "\n")

logError :: String -> Action ()
logError msg = liftIO $ appendFile "error.log" ("ERROR: " ++ msg ++ "\n")
