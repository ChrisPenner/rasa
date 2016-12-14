{-# language ExistentialQuantification, Rank2Types #-}
module Rasa.Run (rasa) where

import Rasa.Ext
import Rasa.State
import Rasa.Alteration

import Control.Lens
import Control.Concurrent.Async
import Control.Monad
import Data.Default (def)

handleEvent :: Store -> Alteration () -> IO Store
handleEvent = execAlteration

rasa :: [Alteration [Event]] -> Alteration () -> IO ()
rasa eventListeners extensions = eventLoop eventListeners extensions def

eventLoop ::  [Alteration [Event]] -> Alteration () -> Store -> IO ()
eventLoop eventListeners extensions store = do
  newStore <- handleEvent store extensions

  unless (newStore^.exiting) $ do
    asyncEventListeners <- traverse (async.evalAlteration newStore) eventListeners
    (_, nextEvents) <- waitAny asyncEventListeners
    let withEvents = (newStore & event .~ nextEvents)
    eventLoop eventListeners extensions withEvents
