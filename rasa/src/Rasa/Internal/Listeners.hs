{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Rasa.Internal.Listeners
  ( onEveryTrigger
  , onEveryTrigger_
  , onInit
  , beforeEveryEvent
  , beforeEveryEvent_
  , beforeEveryRender
  , beforeEveryRender_
  , onEveryRender
  , onEveryRender_
  , afterEveryRender
  , afterEveryRender_
  , dispatchEvent
  , onExit
  , removeListener
  , onBufAdded
  , onBufTextChanged
  ) where


import Rasa.Internal.Action
import Rasa.Internal.Events
import Rasa.Internal.Editor
import Rasa.Internal.Range

import Control.Monad
import Data.Dynamic
import qualified Yi.Rope as Y

-- | This registers an event listener, as long as the listener is well-typed similar to this:
--
-- @MyEventType -> Action ()@ then it will be triggered on all dispatched events of type @MyEventType@.
-- It returns an ID which may be used with 'removeListener' to cancel the listener
onEveryTrigger :: (Typeable event, Typeable response, Monoid response) => (event -> Action response) -> Action ListenerId
onEveryTrigger = addListener

onEveryTrigger_ :: (Typeable event, Typeable response, Monoid response) => (event -> Action response) -> Action ()
onEveryTrigger_ = void . onEveryTrigger

-- | Registers an action to be performed during the Initialization phase.
--
-- This phase occurs exactly ONCE when the editor starts up.
-- Though arbitrary actions may be performed in the configuration block;
-- it's recommended to embed such actions in the onInit event listener
-- so that all event listeners are registered before anything Actions occur.
onInit :: forall a. (Monoid a, Typeable a) => Action a -> Action ()
onInit action = onEveryTrigger_ (const action :: Init -> Action a)

-- | Registers an action to be performed BEFORE each event phase.
beforeEveryEvent :: forall a. (Monoid a, Typeable a) => Action a -> Action ListenerId
beforeEveryEvent action = onEveryTrigger (const action :: BeforeEvent -> Action a)

beforeEveryEvent_ :: forall a. (Monoid a, Typeable a) => Action a -> Action ()
beforeEveryEvent_ = void . beforeEveryEvent

-- | Registers an action to be performed BEFORE each render phase.
--
-- This is a good spot to add information useful to the renderer
-- since all actions have been performed. Only cosmetic changes should
-- occur during this phase.
beforeEveryRender :: forall a. (Monoid a, Typeable a) => Action a -> Action ListenerId
beforeEveryRender action = onEveryTrigger (const action :: BeforeRender -> Action a)

beforeEveryRender_ :: forall a. (Monoid a, Typeable a) => Action a -> Action ()
beforeEveryRender_ = void . beforeEveryRender

-- | Registers an action to be performed during each render phase.
--
-- This phase should only be used by extensions which actually render something.
onEveryRender :: forall a. (Monoid a, Typeable a) => Action a -> Action ListenerId
onEveryRender action = onEveryTrigger (const action :: OnRender -> Action a)

onEveryRender_ :: forall a. (Monoid a, Typeable a) => Action a -> Action ()
onEveryRender_ = void . onEveryRender

-- | Registers an action to be performed AFTER each render phase.
--
-- This is useful for cleaning up extension state that was registered for the
-- renderer, but needs to be cleared before the next iteration.
afterEveryRender :: forall a. (Monoid a, Typeable a) => Action a -> Action ListenerId
afterEveryRender action = onEveryTrigger (const action :: AfterRender -> Action a)

afterEveryRender_ :: forall a. (Monoid a, Typeable a) => Action a -> Action ()
afterEveryRender_ = void . afterEveryRender

-- | Registers an action to be performed during the exit phase.
--
-- This is only triggered exactly once when the editor is shutting down. It
-- allows an opportunity to do clean-up, kill any processes you've started, or
-- save any data before the editor terminates.

onExit :: forall a. (Monoid a, Typeable a) => Action a -> Action ()
onExit action = onEveryTrigger_ (const action :: Exit -> Action a)

-- | Registers an action to be performed after a new buffer is added.
--
-- The supplied function will be called with a 'BufRef' to the new buffer, and the resulting 'Action' will be run.
onBufAdded :: forall a. (Monoid a, Typeable a) => (BufRef -> Action a) -> Action ListenerId
onBufAdded f = onEveryTrigger listener
  where
    listener (BufAdded bRef) = f bRef

-- | This is fired every time text in a buffer changes.
--
-- The range of text which was altered and the new value of that text are provided inside a 'BufTextChanged' event.
onBufTextChanged :: forall a. (Monoid a, Typeable a) => (CrdRange -> Y.YiString -> Action a) -> Action ListenerId
onBufTextChanged f = onEveryTrigger listener
  where
    listener (BufTextChanged r newText) = f r newText
