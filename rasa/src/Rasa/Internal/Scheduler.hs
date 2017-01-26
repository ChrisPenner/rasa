{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Rasa.Internal.Scheduler
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
import Data.Foldable
import Data.Map hiding (filter)
import Unsafe.Coerce
import qualified Yi.Rope as Y

-- | Use this to dispatch an event of any type, any listeners which are listening for this event will be triggered
-- with the provided event. Use this within an Action.
dispatchEvent :: Typeable a => a -> Action ()
dispatchEvent evt = do
  listeners' <- use listeners
  traverse_ ($ evt) (matchingListeners listeners')

-- | This is a helper which extracts and coerces a listener from its wrapper back into the proper event handler type.
getListener :: forall a. Listener -> (a -> Action ())
getListener = coerce
  where
    coerce :: Listener -> (a -> Action ())
    coerce (Listener _ x) = unsafeCoerce x

makeListener :: forall a b. Typeable a => (a -> Action b) -> Action (ListenerId, Listener)
makeListener listenerFunc = do
  n <- nextListenerId <<+= 1
  let listenerId = ListenerId n (typeRep (Proxy :: Proxy a))
      listenerFunc' = void . listenerFunc
  return (listenerId, Listener listenerId listenerFunc')

extendListener :: Listener -> Action () -> Listener
extendListener (Listener listenerId listenerFunc) act = Listener listenerId (\a -> listenerFunc a >> act)

-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners :: forall a. Typeable a => Listeners -> [a -> Action ()]
matchingListeners listeners' = getListener <$> (listeners'^.at (typeRep (Proxy :: Proxy a))._Just)

-- | This registers an event listener listener, as long as the listener is well-typed similar to this:
--
-- @MyEventType -> Action ()@ then it will be triggered on all dispatched events of that type.
-- It returns an ID which may be used with 'removeListener' to cancel the listener
onEveryTrigger :: forall a b. Typeable a => (a -> Action b) -> Action ListenerId
onEveryTrigger listenerFunc = do
  (listenerId, listener) <- makeListener listenerFunc
  listeners %= insertWith mappend (typeRep (Proxy :: Proxy a)) [listener]
  return listenerId

onEveryTrigger_ :: forall a b. Typeable a => (a -> Action b) -> Action ()
onEveryTrigger_ = void . onEveryTrigger

-- | This acts as 'onEveryTrigger' but listens only for the first event of a given type.
onNextEvent :: forall a b. Typeable a => (a -> Action b) -> Action ()
onNextEvent listenerFunc = do
  (listenerId, listener) <- makeListener listenerFunc
  let selfCancellingListener = extendListener listener (removeListener listenerId)
  listeners %= insertWith mappend (typeRep (Proxy :: Proxy a)) [selfCancellingListener]

-- | This removes a listener and prevents it from responding to any more events.
removeListener :: ListenerId -> Action ()
removeListener hkIdA@(ListenerId _ typ) =
  listeners.at typ._Just %= filter listenerMatches
    where
      listenerMatches (Listener hkIdB _) = hkIdA /= hkIdB

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

beforeNextRender :: forall a. Action a -> Action ()
beforeNextRender action = onNextEvent (const action :: BeforeRender -> Action a)

-- | Registers an action to be performed during each render phase.
--
-- This phase should only be used by extensions which actually render something.
onEveryRender :: forall a. Action a -> Action ListenerId
onEveryRender action = onEveryTrigger (const action :: OnRender -> Action a)

onEveryRender_ :: forall a. Action a -> Action ()
onEveryRender_ = void . onEveryRender

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

onBufTextChanged :: forall a. (CrdRange -> Y.YiString -> Action a) -> Action ListenerId
onBufTextChanged f = onEveryTrigger listener
  where
    listener (BufTextChanged r newText) = f r newText
