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
--     onInit  (Runs ONCE)
--
--     -- The following loops until an exit is triggered:
--     beforeEvent -> onEvent -> beforeRender -> onRender -> afterRender
--
--     onExit (Runs ONCE)
-- @
--
-- Each extension which wishes to perform actions exports a @'Scheduler' ()@
-- which the user inserts in their config file.
-- 
-- See an example of a 'Scheduler' and a simple extension here: 'Rasa.Ext'
----------------------------------------------------------------------------

module Rasa.Ext.Scheduler
  (
  -- * Lifecycle Hooks
  S.Scheduler
  , onInit
  , beforeEvent
  , onEvent
  , beforeRender
  , onRender
  , afterRender
  , onExit
  )
    where

import qualified Rasa.Scheduler as S
import Rasa.Action

import Control.Lens
import Control.Monad.Writer
import Data.Default


-- | Registers an action to be performed during the Initialization phase.
--
-- This phase occurs exactly ONCE when the editor starts up.
onInit :: Action () -> S.Scheduler ()
onInit alt = tell (def & S.onInit <>~ [alt])

-- | Registers an action to be performed BEFORE each event phase.
--
-- The 'events' ARE populated at this point.
-- This is a good place to filter out events so they're not seen by other
-- extensions.
beforeEvent :: Action () -> S.Scheduler ()
beforeEvent alt = tell (def & S.beforeEvent <>~ [alt])

-- | Registers an action to be performed during each event phase.
--
-- This is where most extensions should register to run.
onEvent :: Action () -> S.Scheduler ()
onEvent alt = tell (def & S.onEvent <>~ [alt])

-- | Registers an action to be performed BEFORE each render phase.
--
-- This is a good spot to add information useful to the renderer
-- since all actions have been performed. Only cosmetic changes should
-- occur during this phase.
beforeRender :: Action () -> S.Scheduler ()
beforeRender alt = tell (def & S.beforeRender <>~ [alt])

-- | Registers an action to be performed during each render phase.
--
-- This phase should only be used by extensions which actually render something.
onRender :: Action () -> S.Scheduler ()
onRender alt = tell (def & S.onRender <>~ [alt])

-- | Registers an action to be performed AFTER each render phase.
--
-- This is useful for cleaning up extension state that was registered for the
-- renderer, but needs to be cleared before the next iteration.
afterRender :: Action () -> S.Scheduler ()
afterRender alt = tell (def & S.afterRender <>~ [alt])

-- | Registers an action to be performed during the exit phase.
--
-- This is only triggered exactly once when the editor is shutting down. It
-- allows an opportunity to do clean-up, kill any processes you've started, or
-- save any data before the editor terminates.

onExit :: Action () -> S.Scheduler ()
onExit alt = tell (def & S.onExit <>~ [alt])
