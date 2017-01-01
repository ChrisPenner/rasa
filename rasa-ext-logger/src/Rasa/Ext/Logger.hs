module Rasa.Ext.Logger
  ( logger
  , logInfo
  , logError
  ) where

import Rasa.Ext

import Control.Monad.State
import Control.Lens

logger :: Scheduler ()
logger = do
  onInit $ liftIO $ writeFile "logs.log" "Event Log\n"
  afterRender $ do
    ed <- use editor
    liftIO $ appendFile "logs.log" (show ed)

logInfo :: String -> Action ()
logInfo msg = liftIO $ appendFile "info.log" ("INFO: " ++ msg ++ "\n")

logError :: String -> Action ()
logError msg = liftIO $ appendFile "error.log" ("ERROR: " ++ msg ++ "\n")
