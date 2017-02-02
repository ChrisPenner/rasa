{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module Rasa (rasa) where

import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.Events

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
rasa initialize = do
  (output, input) <- spawn unbounded
  evalAction (mkActionState output) $ do
    liftIO $ writeFile "welp.logs" "Start\n"
    initialize
    liftIO $ appendFile "welp.logs" "Init'd\n"
    dispatchEvent Init
    liftIO $ appendFile "welp.logs" "After dispatch\n"
    eventLoop $ fromInput input
    dispatchEvent Exit

-- | This is the main event loop, it runs recursively forever until something
-- sets 'Rasa.Editor.exiting'. It runs the pre-event listeners, then checks if any
-- async events have finished, then runs the post event listeners and repeats.
eventLoop :: Producer (Action ()) IO () -> Action ()
eventLoop producer = do
  dispatchEvent BeforeRender
  dispatchEvent OnRender
  dispatchEvent AfterRender
  dispatchEvent BeforeEvent
  liftIO $ appendFile "welp.logs" "Before event \n"
  (mAction, nextProducer) <- liftIO $ runStateT draw producer
  liftIO $ appendFile "welp.logs" "Before Action \n"
  fromMaybe (return ()) mAction
  isExiting <- use exiting
  unless isExiting $ eventLoop nextProducer
