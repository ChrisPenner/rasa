{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module Rasa.Run (rasa) where

import Rasa.Editor
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
    dispatchEvent Init
    eventLoop eventListeners
    dispatchEvent Exit
    where hooks = getHooks scheduler

eventLoop :: [Action [Keypress]] -> Action ()
eventLoop eventListeners = do
  dispatchEvent BeforeRender
  dispatchEvent OnRender
  dispatchEvent AfterRender
  dispatchEvent BeforeEvent
  currentEditor <- get
  hooks <- ask
  -- This is a little weird, but I think it needs to be this way to execute listeners in parallel
  asyncEventListeners <- liftIO $ traverse (async.evalAction currentEditor hooks) eventListeners
  (_, nextEvents) <- liftIO $ waitAny asyncEventListeners
  traverse_ dispatchEvent nextEvents
  isExiting <- use exiting
  unless isExiting $ eventLoop eventListeners
