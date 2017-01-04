{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module Rasa (rasa) where

import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.Events
import Rasa.Internal.Scheduler

import Pipes
import Pipes.Concurrent
import Pipes.Parse

import Control.Lens
import Control.Monad

import Data.Default (def)
import Data.Maybe

-- | The main function to run rasa.
--
-- @rasa eventProviders extensions@
--
-- This should be imported by a user-config with and called with a 'Scheduler'
-- containing any extensions which have event listeners.
--
-- > rasa $ do
-- >   cursor
-- >   vim
-- >   slate

rasa :: Scheduler () -> IO ()
rasa scheduler = do
  (output, input) <- spawn unbounded
  evalAction (ActionState def output) hooks $ do
    dispatchEvent Init
    eventLoop $ fromInput input
    dispatchEvent Exit
    where hooks = getHooks scheduler

-- | This is the main event loop, it runs recursively forever until something
-- sets 'Rasa.Editor.exiting', or there are no longer any event providers. It
-- runs the pre-event hooks, then checks if any async events have finished,
-- then runs the post event hooks and repeats.

eventLoop :: Producer (Action ()) IO () -> Action ()
eventLoop producer = do
  dispatchEvent BeforeRender
  dispatchEvent OnRender
  dispatchEvent AfterRender
  dispatchEvent BeforeEvent
  (mAction, nextProducer) <- liftIO $ runStateT draw producer
  fromMaybe (exiting .= False) mAction
  isExiting <- use exiting
  unless isExiting (eventLoop nextProducer)
