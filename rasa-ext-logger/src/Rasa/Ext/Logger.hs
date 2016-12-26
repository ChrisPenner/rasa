module Rasa.Ext.Logger
  ( logger
  , logInfo
  , logError
  ) where

import Rasa.Ext
import Rasa.Ext.Scheduler
import Rasa.State

import Control.Lens
import Control.Monad.State

logger :: Scheduler ()
logger = do
  onInit $ liftIO $ writeFile "logs.log" "Event Log\n"
  afterRender $ do
    bufs <- use buffers
    extensions <- use extState
    liftIO $ appendFile "logs.log" ("Buffers==============\n" ++ show bufs ++ "\n\n")
    liftIO $ appendFile "logs.log" ("Editor Extensions==============\n" ++ show extensions ++ "\n\n")
    liftIO $ appendFile "logs.log" "---\n\n"

logInfo :: String -> Action ()
logInfo msg = liftIO $ appendFile "info.log" ("INFO: " ++ msg ++ "\n")

logError :: String -> Action ()
logError msg = liftIO $ appendFile "error.log" ("ERROR: " ++ msg ++ "\n")
