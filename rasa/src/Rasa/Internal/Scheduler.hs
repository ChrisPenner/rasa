{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Rasa.Internal.Scheduler
  ( Hook
  , Hooks
  , afterRender
  , beforeEvent
  , beforeRender
  , dispatchEvent
  , eventListener
  , removeListener
  , matchingHooks
  , onInit
  , onExit
  , onRender
  , onBufAdded
  ) where


import Rasa.Internal.Action
import Rasa.Internal.Events
import Rasa.Internal.Editor

import Control.Lens
import Data.Dynamic
import Data.Foldable
import Data.Map hiding (filter)
import Unsafe.Coerce

-- | Use this to dispatch an event of any type, any hooks which are listening for this event will be triggered
-- with the provided event. Use this within an Action.
dispatchEvent :: Typeable a => a -> Action ()
dispatchEvent evt = do
  hooks' <- use hooks
  traverse_ ($ evt) (matchingHooks hooks')

-- | This is a helper which extracts and coerces a hook from its wrapper back into the proper event handler type.
getHook :: forall a. Hook -> (a -> Action ())
getHook = coerce
  where
    coerce :: Hook -> (a -> Action ())
    coerce (Hook _ x) = unsafeCoerce x

makeHook :: forall a. Typeable a => (a -> Action ()) -> Action (HookId, Hook)
makeHook hookFunc = do
  n <- nextHook <<+= 1
  let hookId = HookId n (typeRep (Proxy :: Proxy a))
  return (hookId, Hook hookId hookFunc)

-- | This extracts all event listener hooks from a map of hooks which match the type of the provided event.
matchingHooks :: forall a. Typeable a => Hooks -> [a -> Action ()]
matchingHooks hooks' = getHook <$> (hooks'^.at (typeRep (Proxy :: Proxy a))._Just)

-- | This registers an event listener hook, as long as the listener is well-typed similar to this:
--
-- @MyEventType -> Action ()@ then it will be registered to listen for dispatched events of that type.
-- Use within the 'Rasa.Internal.Scheduler.Scheduler' and add have the user add it to their config.
-- It returns an ID which may be used with 'removeListener'
eventListener :: forall a. Typeable a => (a -> Action ()) -> Action HookId
eventListener hookFunc = do
  (hookId, hook) <- makeHook hookFunc
  hooks %= insertWith mappend (typeRep (Proxy :: Proxy a)) [hook]
  return hookId

-- | This removes a listener and prevents it from responding to any more events.
removeListener :: HookId -> Action ()
removeListener hkIdA@(HookId _ typ) =
  hooks.at typ._Just %= filter hookMatches
    where
      hookMatches (Hook hkIdB _) = hkIdA /= hkIdB

-- | Registers an action to be performed during the Initialization phase.
--
-- This phase occurs exactly ONCE when the editor starts up.
-- Though arbitrary actions may be performed in the configuration block;
-- it's recommended to embed such actions in the onInit event listener
-- so that all event listeners are registered before anything Actions occur.
onInit :: Action () -> Action HookId
onInit action = eventListener (const action :: Init -> Action ())

-- | Registers an action to be performed BEFORE each event phase.
beforeEvent :: Action () -> Action HookId
beforeEvent action = eventListener (const action :: BeforeEvent -> Action ())

-- | Registers an action to be performed BEFORE each render phase.
--
-- This is a good spot to add information useful to the renderer
-- since all actions have been performed. Only cosmetic changes should
-- occur during this phase.
beforeRender :: Action () -> Action HookId
beforeRender action = eventListener (const action :: BeforeRender -> Action ())

-- | Registers an action to be performed during each render phase.
--
-- This phase should only be used by extensions which actually render something.
onRender :: Action () -> Action HookId
onRender action = eventListener (const action :: OnRender -> Action ())

-- | Registers an action to be performed AFTER each render phase.
--
-- This is useful for cleaning up extension state that was registered for the
-- renderer, but needs to be cleared before the next iteration.
afterRender :: Action () -> Action HookId
afterRender action = eventListener (const action :: AfterRender -> Action ())

-- | Registers an action to be performed during the exit phase.
--
-- This is only triggered exactly once when the editor is shutting down. It
-- allows an opportunity to do clean-up, kill any processes you've started, or
-- save any data before the editor terminates.

onExit :: Action () -> Action HookId
onExit action = eventListener (const action :: Exit -> Action ())

-- | Registers an action to be performed after a new buffer is added.
--
-- The supplied function will be called with a 'BufRef' to the new buffer, and the resulting 'Action' will be run.
onBufAdded :: (BufRef -> Action ()) -> Action HookId
onBufAdded f = eventListener listener
  where
    listener (BufAdded bRef) = f bRef
