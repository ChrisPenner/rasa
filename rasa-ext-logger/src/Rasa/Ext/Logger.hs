module Rasa.Ext.Logger (logger) where

import Rasa.State
import Rasa.Alteration
import Rasa.Event

import Control.Lens
import Control.Monad
import Control.Monad.State

logger :: Alteration ()
logger = do
  evt <- use event
  liftIO $ when (Init `elem` evt) (writeFile "logs.log" "Event Log\n")

  bufs <- use buffers
  extensions <- use extState

  liftIO $ appendFile "logs.log" ("Events==============\n" ++ show evt ++ "\n\n")
  liftIO $ appendFile "logs.log" ("Buffers==============\n" ++ show bufs ++ "\n\n")
  liftIO $ appendFile "logs.log" ("Editor Extensions==============\n" ++ show extensions ++ "\n\n")
  liftIO $ appendFile "logs.log" "---\n\n"
