{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module Rasa (rasa) where

import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.Events
import Rasa.Internal.Scheduler

import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Default (def)
import Data.List

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
rasa :: Scheduler () -> IO ()
rasa scheduler =
  evalAction def hooks $ do
    dispatchEvent Init
    eventLoop
    dispatchEvent Exit
    where hooks = getHooks scheduler

-- | This is the main event loop, it runs recursively forever until something
-- sets 'Rasa.Editor.exiting'. It runs the pre-event hooks, then listens for an
-- event from the event providers, then runs the post event hooks and repeats.
eventLoop :: Action ()
eventLoop = do
  dispatchEvent BeforeRender
  dispatchEvent OnRender
  dispatchEvent AfterRender
  dispatchEvent BeforeEvent
  asyncActions <- use asyncs
  (done, action) <- liftIO $ waitAny asyncActions
  asyncs %= delete done
  action
  isExiting <- use exiting
  unless isExiting eventLoop
