{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Rasa.Internal.Listeners
  ( Listener
  , Listeners
  , onEveryTrigger
  , onEveryTrigger_
  , onNextEvent
  , onInit
  , beforeEveryEvent
  , beforeEveryEvent_
  , beforeNextEvent
  , beforeEveryRender
  , beforeEveryRender_
  , beforeNextRender
  , onEveryRender
  , onEveryRender_
  , onNextRender
  , afterEveryRender
  , afterEveryRender_
  , afterNextRender
  , dispatchEvent
  , onExit
  , removeListener
  , matchingListeners
  , onBufAdded
  , onBufTextChanged
  ) where


import Rasa.Internal.Action
import Rasa.Internal.Events
import Rasa.Internal.Editor
import Rasa.Internal.Range

import Control.Lens
import Control.Monad
import Data.Dynamic
import Unsafe.Coerce
import qualified Yi.Rope as Y

-- | Use this to dispatch an event of any type, any listeners which are listening for this event will be triggered
-- with the provided event. Use this within an Action.
-- dispatchEvent :: Typeable a => a -> Action ()
-- dispatchEvent evt = do
  -- listeners' <- use listeners
  -- traverse_ ($ evt) (matchingListeners listeners')

-- | This is a helper which extracts and coerces a listener from its wrapper back into the proper event handler type.
getListener :: forall a. Listener -> (a -> Action ())
getListener = coerce
  where
    coerce :: Listener -> (a -> Action ())
    coerce (Listener _ x) = unsafeCoerce x

extendListener :: Listener -> Action () -> Listener
extendListener (Listener listenerId listenerFunc) act = Listener listenerId (\a -> listenerFunc a >> act)

-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners :: forall a. Typeable a => Listeners -> [a -> Action ()]
matchingListeners listeners' = getListener <$> (listeners'^.at (typeRep (Proxy :: Proxy a))._Just)

-- | This registers an event listener, as long as the listener is well-typed similar to this:
--
-- @MyEventType -> Action ()@ then it will be triggered on all dispatched events of type @MyEventType@.
-- It returns an ID which may be used with 'removeListener' to cancel the listener
onEveryTrigger :: Typeable event => (event -> Action b) -> Action ListenerId
onEveryTrigger = addListener

onEveryTrigger_ :: Typeable event => (event -> Action b) -> Action ()
onEveryTrigger_ = void . onEveryTrigger

-- | This acts as 'onEveryTrigger' but listens only for the first event of a given type.
onNextEvent :: forall a b. Typeable a => (a -> Action b) -> Action ()
onNextEvent listenerFunc = undefined -- do
  -- (listenerId, listener) <- makeListener listenerFunc
  -- let selfCancellingListener = extendListener listener (removeListener listenerId)
  -- listeners %= insertWith mappend (typeRep (Proxy :: Proxy a)) [selfCancellingListener]

-- | Registers an action to be performed during the Initialization phase.
--
-- This phase occurs exactly ONCE when the editor starts up.
-- Though arbitrary actions may be performed in the configuration block;
-- it's recommended to embed such actions in the onInit event listener
-- so that all event listeners are registered before anything Actions occur.
onInit :: forall a. Action a -> Action ()
onInit action = onNextEvent (const action :: Init -> Action a)

-- | Registers an action to be performed BEFORE each event phase.
beforeEveryEvent :: forall a. Action a -> Action ListenerId
beforeEveryEvent action = onEveryTrigger (const action :: BeforeEvent -> Action a)

beforeEveryEvent_ :: forall a. Action a -> Action ()
beforeEveryEvent_ = void . beforeEveryEvent

-- | Registers an action to be performed ONCE before only the NEXT event phase.
beforeNextEvent :: forall a. Action a -> Action ()
beforeNextEvent action = onNextEvent (const action :: BeforeEvent -> Action a)

-- | Registers an action to be performed BEFORE each render phase.
--
-- This is a good spot to add information useful to the renderer
-- since all actions have been performed. Only cosmetic changes should
-- occur during this phase.
beforeEveryRender :: forall a. Action a -> Action ListenerId
beforeEveryRender action = onEveryTrigger (const action :: BeforeRender -> Action a)

beforeEveryRender_ :: forall a. Action a -> Action ()
beforeEveryRender_ = void . beforeEveryRender

-- | Registers an action to be performed ONCE before only the NEXT render phase.
beforeNextRender :: forall a. Action a -> Action ()
beforeNextRender action = onNextEvent (const action :: BeforeRender -> Action a)

-- | Registers an action to be performed during each render phase.
--
-- This phase should only be used by extensions which actually render something.
onEveryRender :: forall a. Action a -> Action ListenerId
onEveryRender action = onEveryTrigger (const action :: OnRender -> Action a)

onEveryRender_ :: forall a. Action a -> Action ()
onEveryRender_ = void . onEveryRender

-- | Registers an action to be performed ONCE before only the NEXT render phase.
--
-- This phase should only be used by extensions which actually render something.
onNextRender :: forall a. Action a -> Action ()
onNextRender action = onNextEvent (const action :: OnRender -> Action a)

-- | Registers an action to be performed AFTER each render phase.
--
-- This is useful for cleaning up extension state that was registered for the
-- renderer, but needs to be cleared before the next iteration.
afterEveryRender :: forall a. Action a -> Action ListenerId
afterEveryRender action = onEveryTrigger (const action :: AfterRender -> Action a)

afterEveryRender_ :: forall a. Action a -> Action ()
afterEveryRender_ = void . afterEveryRender

-- | Registers an action to be performed after the NEXT render phase.
afterNextRender :: forall a. Action a -> Action ()
afterNextRender action = onNextEvent (const action :: AfterRender -> Action a)

-- | Registers an action to be performed during the exit phase.
--
-- This is only triggered exactly once when the editor is shutting down. It
-- allows an opportunity to do clean-up, kill any processes you've started, or
-- save any data before the editor terminates.

onExit :: forall a. Action a -> Action ()
onExit action = onNextEvent (const action :: Exit -> Action a)

-- | Registers an action to be performed after a new buffer is added.
--
-- The supplied function will be called with a 'BufRef' to the new buffer, and the resulting 'Action' will be run.
onBufAdded :: forall a. (BufRef -> Action a) -> Action ListenerId
onBufAdded f = onEveryTrigger listener
  where
    listener (BufAdded bRef) = f bRef

-- | This is fired every time text in a buffer changes.
--
-- The range of text which was altered and the new value of that text are provided inside a 'BufTextChanged' event.
onBufTextChanged :: forall a. (CrdRange -> Y.YiString -> Action a) -> Action ListenerId
onBufTextChanged f = onEveryTrigger listener
  where
    listener (BufTextChanged r newText) = f r newText
