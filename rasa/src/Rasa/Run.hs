{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module Rasa.Run (rasa) where

import Rasa.State
import Rasa.Action
import Rasa.Events
import Rasa.Scheduler

import Control.Lens
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Default (def)
import Data.Foldable

rasa :: [Action [Keypress]] -> Scheduler () -> IO ()
rasa eventListeners scheduler =
  evalAction def hooks $ do
    handleEvent Init
    eventLoop eventListeners
    handleEvent Exit
    where hooks = getHooks scheduler

eventLoop :: [Action [Keypress]] -> Action ()
eventLoop eventListeners = do
  handleEvent BeforeRender
  handleEvent OnRender
  handleEvent AfterRender
  handleEvent BeforeEvent
  currentState <- get
  hooks <- ask
  -- This is a little weird, but I think it needs to be this way to execute listeners in parallel
  asyncEventListeners <- liftIO $ traverse (async.evalAction currentState hooks) eventListeners
  (_, nextEvents) <- liftIO $ waitAny asyncEventListeners
  traverse_ handleEvent nextEvents
  isExiting <- use exiting
  unless isExiting $ eventLoop eventListeners
