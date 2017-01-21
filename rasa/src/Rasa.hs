{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module Rasa (rasa) where

import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.Events
import Rasa.Internal.Scheduler

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe

import Pipes
import Pipes.Concurrent
import Pipes.Parse

-- | The main function to run rasa.
--
-- @rasa eventProviders extensions@
--
-- This should be imported by a user-config with and called with an 'Action'
-- containing any extensions which have event listeners.
--
-- > rasa $ do
-- >   cursor
-- >   vim
-- >   slate

rasa :: Action () -> IO ()
rasa initilize = do
  (output, input) <- spawn unbounded
  evalAction (mkActionState output) $ do
    initilize
    dispatchEvent Init
    eventLoop $ fromInput input
    dispatchEvent Exit

-- | This is the main event loop, it runs recursively forever until something
-- sets 'Rasa.Editor.exiting'. It runs the pre-event hooks, then checks if any
-- async events have finished, then runs the post event hooks and repeats.
eventLoop :: Producer (Action ()) IO () -> Action ()
eventLoop producer = do
  dispatchEvent BeforeRender
  dispatchEvent OnRender
  dispatchEvent AfterRender
  dispatchEvent BeforeEvent
  (mAction, nextProducer) <- liftIO $ runStateT draw producer
  fromMaybe (return ()) mAction
  isExiting <- use exiting
  unless isExiting $ eventLoop nextProducer
