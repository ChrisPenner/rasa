module Rasa.Ext.Logger (logger) where

import Rasa.Ext
import Rasa.Ext.Scheduler
import Rasa.State

import Control.Lens
import Control.Monad.State

logger :: Scheduler ()
logger = do
  onInit $ liftIO $ writeFile "logs.log" "Event Log\n"
  afterRender $ do
    evts <- use events
    bufs <- use buffers
    extensions <- use extState

    liftIO $ appendFile "logs.log" ("Events==============\n" ++ show evts ++ "\n\n")
    liftIO $ appendFile "logs.log" ("Buffers==============\n" ++ show bufs ++ "\n\n")
    liftIO $ appendFile "logs.log" ("Editor Extensions==============\n" ++ show extensions ++ "\n\n")
    liftIO $ appendFile "logs.log" "---\n\n"
