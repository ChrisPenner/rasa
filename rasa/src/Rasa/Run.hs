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
import Data.Dynamic
import Data.Foldable

processAction :: forall a. Typeable a => Hooks -> a -> Action ()
processAction hooks evt = traverse_ ($ evt) matchingHooks
  where
    matchingHooks :: [a -> Action ()]
    matchingHooks = hooks^.at (typeRep (Proxy :: Proxy a))._Just.to getHook

doThing :: Hooks ->  Action ()
doThing hooks = do
  evts <- use events
  traverse_ (recurse.processAction hooks) evts
    where recurse act = do
            act
            hasEvents <- use (events.to null)
            when hasEvents (doThing hooks)


rasa :: [Action [Event]] -> Scheduler () -> IO ()
rasa eventListeners scheduler = do
  initStore <- execAction (def & events .~ [Event Init]) mainAction
  lastStore <- eventLoop eventListeners mainAction initStore
  void $ execAction (lastStore & events .~ [Event Exit]) mainAction
    where hooks = getHooks scheduler
          mainAction = doThing hooks

eventLoop ::  [Action [Event]] -> Action () -> Store -> IO Store
eventLoop eventListeners mainAction store = do
  asyncEventListeners <- traverse (async.evalAction store) eventListeners
  (_, nextEvents) <- waitAny asyncEventListeners
  newStore <- execAction (store & events .~ [Event Exit]) mainAction
  if newStore^.exiting 
     then return newStore
     else eventLoop eventListeners mainAction (newStore & events .~ nextEvents)
