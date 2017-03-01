{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module Rasa (rasa) where

import Reflex
import Rasa.Internal.Listeners

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

rasa :: App () -> IO ()
rasa = void . reflex

-- rasa :: Action () -> IO ()
-- rasa initialize = do
--   (output, input) <- spawn unbounded
--   evalAction (mkActionState output) $ do
--     initialize
--     dispatchInit
--     dispatchAfterInit
--     eventLoop $ fromInput input
--     dispatchExit

-- -- | This is the main event loop, it runs recursively forever until something
-- -- sets 'Rasa.Editor.exiting'. It runs the pre-event listeners, then checks if any
-- -- async events have finished, then runs the post event listeners and repeats.
-- eventLoop :: Producer (Action ()) IO () -> Action ()
-- eventLoop producer = do
--   dispatchBeforeRender
--   dispatchOnRender
--   dispatchAfterRender
--   dispatchBeforeEvent
--   (mAction, nextProducer) <- liftIO $ runStateT draw producer
--   fromMaybe (return ()) mAction
--   isExiting <- shouldExit
--   unless isExiting $ eventLoop nextProducer
