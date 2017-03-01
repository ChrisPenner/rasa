module Rasa.Ext.Logger
  ( logger
  , logInfo
  , logError
  ) where

import Rasa.Ext

import Control.Monad.State

logger :: App ()
logger = do
  onInit $ liftIO $ writeFile "logs.log" "Event Log\n"
  onEveryRender_ $ return ()
    -- ed <- getEditor
    -- liftIO $ appendFile "logs.log" (show ed)

logInfo :: String -> App ()
logInfo msg = liftIO $ appendFile "info.log" ("INFO: " ++ msg ++ "\n")

logError :: String -> App ()
logError msg = liftIO $ appendFile "error.log" ("ERROR: " ++ msg ++ "\n")
