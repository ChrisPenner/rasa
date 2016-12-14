{-# language ExistentialQuantification, Rank2Types #-}
module Rasa.Run (rasa) where

import Rasa.State
import Rasa.Alteration
import Rasa.Events
import Rasa.Scheduler

import Control.Lens
import Control.Concurrent.Async
import Control.Monad
import Data.Default (def)

rasa :: [Alteration [Event]] -> Scheduler () -> IO ()
rasa eventListeners scheduler = do
  initStore <- execAlteration def initialization
  lastStore <- eventLoop eventListeners actions initStore
  void $ execAlteration lastStore finalization
    where schedule = getSchedule scheduler
          actions = runSchedule schedule
          initialization = sequence_ (schedule^.onInit)
          finalization = sequence_ (schedule^.onExit)

eventLoop ::  [Alteration [Event]] -> Alteration () -> Store -> IO Store
eventLoop eventListeners actions store = do
  newStore <- execAlteration store actions
  if newStore^.exiting 
     then return newStore
     else do
       asyncEventListeners <- traverse (async.evalAlteration newStore) eventListeners
       (_, nextEvents) <- waitAny asyncEventListeners
       let withEvents = newStore & event .~ nextEvents
       eventLoop eventListeners actions withEvents
