{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Rasa.Internal.Listeners
  ( onInit
  , dispatchInit
  , beforeEveryRender
  , beforeEveryRender_
  , dispatchBeforeRender
  , beforeEveryEvent
  , beforeEveryEvent_
  , dispatchBeforeEvent
  , onEveryRender
  , onEveryRender_
  , dispatchOnRender
  , afterEveryRender
  , afterEveryRender_
  , dispatchAfterRender
  , onExit
  , dispatchExit
  , onBufAdded
  , dispatchBufAdded
  , onBufTextChanged

  , onKeypress
  , dispatchKeypress
  , bufTextChanged
  ) where


import Rasa.Internal.Action
import Rasa.Internal.Events

import Control.Monad

onKeypress :: (Keypress -> Action ()) -> Action ListenerId
onKeypress = mkRegistrar

dispatchKeypress :: Keypress -> Action ()
dispatchKeypress = mkDispatcher

-- | This registers an event listener, as long as the listener is well-typed similar to this:
--
-- @MyEventType -> Action ()@ then it will be triggered on all dispatched events of type @MyEventType@.
-- It returns an ID which may be used with 'removeListener' to cancel the listener
-- onEveryTrigger :: Typeable event => (event -> Action b) -> Action ListenerId
-- onEveryTrigger = addListener

-- onEveryTrigger_ :: Typeable event => (event -> Action b) -> Action ()
-- onEveryTrigger_ = void . onEveryTrigger

-- | Registers an action to be performed during the Initialization phase.
--
-- This phase occurs exactly ONCE when the editor starts up.
-- Though arbitrary actions may be performed in the configuration block;
-- it's recommended to embed such actions in the onInit event listener
-- so that all event listeners are registered before anything Actions occur.
onInit :: Action a -> Action ()
onInit action = void $ mkRegistrar (const (void action) :: Init -> Action ())

dispatchInit :: Action ()
dispatchInit = mkDispatcher Init

-- | Registers an action to be performed BEFORE each event phase.
beforeEveryEvent :: Action a -> Action ListenerId
beforeEveryEvent action = mkRegistrar (const (void action) :: BeforeEvent -> Action ())

beforeEveryEvent_ :: Action a -> Action ()
beforeEveryEvent_ = void . beforeEveryEvent

dispatchBeforeEvent :: Action ()
dispatchBeforeEvent = mkDispatcher BeforeEvent

-- | Registers an action to be performed BEFORE each render phase.
--
-- This is a good spot to add information useful to the renderer
-- since all actions have been performed. Only cosmetic changes should
-- occur during this phase.
beforeEveryRender :: Action a -> Action ListenerId
beforeEveryRender action = mkRegistrar (const (void action) :: BeforeRender -> Action ())

beforeEveryRender_ :: Action a -> Action ()
beforeEveryRender_ = void . beforeEveryRender

dispatchBeforeRender :: Action ()
dispatchBeforeRender = mkDispatcher BeforeRender

-- | Registers an action to be performed during each render phase.
--
-- This phase should only be used by extensions which actually render something.
onEveryRender :: Action a -> Action ListenerId
onEveryRender action = mkRegistrar (const $ void action :: OnRender -> Action ())

onEveryRender_ :: Action a -> Action ()
onEveryRender_ = void . onEveryRender

dispatchOnRender :: Action ()
dispatchOnRender = mkDispatcher OnRender

-- | Registers an action to be performed AFTER each render phase.
--
-- This is useful for cleaning up extension state that was registered for the
-- renderer, but needs to be cleared before the next iteration.
afterEveryRender :: Action a -> Action ListenerId
afterEveryRender action = mkRegistrar (const $ void action :: AfterRender -> Action ())

afterEveryRender_ :: Action a -> Action ()
afterEveryRender_ = void . afterEveryRender

dispatchAfterRender :: Action ()
dispatchAfterRender = mkDispatcher AfterRender

-- | Registers an action to be performed during the exit phase.
--
-- This is only triggered exactly once when the editor is shutting down. It
-- allows an opportunity to do clean-up, kill any processes you've started, or
-- save any data before the editor terminates.

onExit :: Action a -> Action ()
onExit action = void $ mkRegistrar (const $ void action :: Exit -> Action ())

dispatchExit :: Action ()
dispatchExit = mkDispatcher Exit

-- | Registers an action to be performed after a new buffer is added.
--
-- The supplied function will be called with a 'BufRef' to the new buffer, and the resulting 'Action' will be run.
onBufAdded :: (BufAdded -> Action a) -> Action ListenerId
onBufAdded actionF = mkRegistrar $ void <$> actionF

dispatchBufAdded :: BufAdded -> Action ()
dispatchBufAdded = mkDispatcher

-- | This is fired every time text in a buffer changes.
--
-- The range of text which was altered and the new value of that text are provided inside a 'BufTextChanged' event.
onBufTextChanged :: (BufTextChanged -> Action a) -> Action ListenerId
onBufTextChanged actionF = mkRegistrar $ void <$> actionF

bufTextChanged :: BufTextChanged -> Action ()
bufTextChanged = mkDispatcher
