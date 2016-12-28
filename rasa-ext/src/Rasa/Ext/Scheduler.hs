----------------------------------------------------------------------------
-- |
-- Module      :  Rasa.Ext.Scheduler
-- Copyright   :  (C) 2016 Chris Penner
-- License     :  MIT
-- Maintainer  :  Chris Penner <christopher.penner@gmail.com>
--
-- The Scheduler is how you can register your extension's actions to run
-- at different points in the editor's event cycle.
--
-- The event cycle proceeds as follows:
--
-- @
--     Init  (Runs ONCE)
--
--     -- The following loops until an exit is triggered:
--     BeforeEvent -> (any event) -> BeforeRender -> OnRender -> AfterRender
--
--     Exit (Runs ONCE)
-- @
--
-- Each extension which wishes to perform actions exports a @'Scheduler' ()@
-- which the user inserts in their config file.
-- 
-- See an example of a 'Scheduler' and a simple extension in the "Rasa.Ext" module.
----------------------------------------------------------------------------

module Rasa.Ext.Scheduler
  (
  -- * Lifecycle Hooks
  Scheduler
  , Hooks
  , Hook
  , dispatchEvent
  , eventListener
  , onInit
  , beforeEvent
  , beforeRender
  , onRender
  , afterRender
  , onExit
  ) where

import Rasa.Scheduler
import Rasa.Action
import Rasa.Events

-- | Registers an action to be performed during the Initialization phase.
--
-- This phase occurs exactly ONCE when the editor starts up.
onInit :: Action () -> Scheduler ()
onInit action = eventListener (const action :: Init -> Action ())

-- | Registers an action to be performed BEFORE each event phase.
beforeEvent :: Action () -> Scheduler ()
beforeEvent action = eventListener (const action :: BeforeEvent -> Action ())

-- | Registers an action to be performed BEFORE each render phase.
--
-- This is a good spot to add information useful to the renderer
-- since all actions have been performed. Only cosmetic changes should
-- occur during this phase.
beforeRender :: Action () -> Scheduler ()
beforeRender action = eventListener (const action :: BeforeRender -> Action ())

-- | Registers an action to be performed during each render phase.
--
-- This phase should only be used by extensions which actually render something.
onRender :: Action () -> Scheduler ()
onRender action = eventListener (const action :: OnRender -> Action ())

-- | Registers an action to be performed AFTER each render phase.
--
-- This is useful for cleaning up extension state that was registered for the
-- renderer, but needs to be cleared before the next iteration.
afterRender :: Action () -> Scheduler ()
afterRender action = eventListener (const action :: AfterRender -> Action ())

-- | Registers an action to be performed during the exit phase.
--
-- This is only triggered exactly once when the editor is shutting down. It
-- allows an opportunity to do clean-up, kill any processes you've started, or
-- save any data before the editor terminates.

onExit :: Action () -> Scheduler ()
onExit action = eventListener (const action :: Exit -> Action ())
