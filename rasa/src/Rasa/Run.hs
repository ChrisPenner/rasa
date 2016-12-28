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

-- | The main function to run rasa.
-- 
-- @rasa eventProviders extensions@
-- 
-- This should be imported by a user-config and called with extension event providers and extension event hooks as
-- arguments. e.g.:
-- 
-- > rasa [slateEvent] $ do
-- >   cursor
-- >   vim
rasa :: [Action [Keypress]] -> Scheduler () -> IO ()
rasa eventProviders scheduler =
  evalAction def hooks $ do
    dispatchEvent Init
    eventLoop eventProviders
    dispatchEvent Exit
    where hooks = getHooks scheduler

-- | This is the main event loop, it runs recursively forever until something
-- sets 'Rasa.Editor.exiting'. It runs the pre-event hooks, then listens for an
-- event from the event providers, then runs the post event hooks and repeats.
eventLoop :: [Action [Keypress]] -> Action ()
eventLoop eventProviders = do
  dispatchEvent BeforeRender
  dispatchEvent OnRender
  dispatchEvent AfterRender
  dispatchEvent BeforeEvent
  currentEditor <- get
  hooks <- ask
  -- This is a little weird, but I think it needs to be this way to execute providers in parallel
  asyncEventProviders <- liftIO $ traverse (async.evalAction currentEditor hooks) eventProviders
  (_, nextEvents) <- liftIO $ waitAny asyncEventProviders
  traverse_ dispatchEvent nextEvents
  isExiting <- use exiting
  unless isExiting $ eventLoop eventProviders
