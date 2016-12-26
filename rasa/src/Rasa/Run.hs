{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module Rasa.Run (rasa) where

import Rasa.State
import Rasa.Action
import Rasa.Events
import Rasa.Scheduler

import Control.Lens
import Control.Concurrent.Async
import Control.Monad
import Data.Default (def)
import Data.Foldable
import Data.Dynamic

performAction :: Typeable a => Hooks -> a -> Action ()
performAction hooks evt = traverse_ ($ evt) (matchingHooks hooks)

rasa :: [Action [Keypress]] -> Scheduler () -> IO ()
rasa eventListeners scheduler = do
  initStore <- execAction def (performAction hooks Init)
  lastStore <- eventLoop eventListeners hooks initStore
  void $ execAction lastStore (performAction hooks Exit)
    where hooks = getHooks scheduler

eventLoop :: [Action [Keypress]] -> Hooks -> Store -> IO Store
eventLoop eventListeners hooks store = do
  afterRenderStore <- doHooks [BeforeRender] store
                >>= doHooks [OnRender]
                >>= doHooks [AfterRender]
  beforeEventStore <- doHooks [BeforeEvent] afterRenderStore
  asyncEventListeners <- traverse (async.evalAction beforeEventStore) eventListeners
  (_, nextEvents) <- waitAny asyncEventListeners
  afterEventsStore <- doHooks nextEvents beforeEventStore
  if afterEventsStore^.exiting 
     then return afterEventsStore
     else eventLoop eventListeners hooks afterEventsStore
       where doHooks :: Typeable a => [a] -> Store -> IO Store
             doHooks events store' = execAction store' $ traverse_ (performAction hooks) events
