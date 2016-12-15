{-# language ExistentialQuantification, Rank2Types #-}
module Rasa.Run (rasa) where

import Rasa.State
import Rasa.Action
import Rasa.Events
import Rasa.Scheduler

import Control.Lens
import Control.Concurrent.Async
import Control.Monad
import Data.Default (def)

rasa :: [Action [Event]] -> Scheduler () -> IO ()
rasa eventListeners scheduler = do
  initStore <- execAction def initialization
  lastStore <- eventLoop eventListeners actions initStore
  void $ execAction lastStore finalization
    where schedule = getSchedule scheduler
          actions = runSchedule schedule
          initialization = sequence_ (schedule^.onInit)
          finalization = sequence_ (schedule^.onExit)

eventLoop ::  [Action [Event]] -> Action () -> Store -> IO Store
eventLoop eventListeners actions store = do
  newStore <- execAction store actions
  if newStore^.exiting 
     then return newStore
     else do
       asyncEventListeners <- traverse (async.evalAction newStore) eventListeners
       (_, nextEvents) <- waitAny asyncEventListeners
       let withEvents = newStore & event .~ nextEvents
       eventLoop eventListeners actions withEvents
