{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving,
   ExistentialQuantification, ScopedTypeVariables #-}

module Rasa.Internal.Scheduler
  ( Scheduler(..)
  , Hook
  , Hooks
  , afterRender
  , beforeEvent
  , beforeRender
  , dispatchEvent
  , eventListener
  , getHooks
  , matchingHooks
  , onExit
  , onInit
  , onRender
  ) where


import Rasa.Internal.Action
import Rasa.Internal.Events

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Dynamic
import Data.Foldable
import Data.Map
import Unsafe.Coerce

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
newtype Scheduler a = Scheduler
  { runSched :: State Hooks a
  } deriving (Functor, Applicative, Monad, MonadState Hooks)

-- | Use this to dispatch an event of any type, any hooks which are listening for this event will be triggered
-- with the provided event. Use this within an Action.
dispatchEvent :: Typeable a => a -> Action ()
dispatchEvent evt = do
  hooks <- ask
  traverse_ ($ evt) (matchingHooks hooks)

-- | This is a helper which extracts and coerces a hook from its wrapper back into the proper event handler type.
getHook :: forall a. Hook -> (a -> Action ())
getHook = coerce
  where
    coerce :: Hook -> (a -> Action ())
    coerce (Hook x) = unsafeCoerce x

-- | This extracts all event listener hooks from a map of hooks which match the type of the provided event.
matchingHooks :: forall a. Typeable a => Hooks -> [a -> Action ()]
matchingHooks hooks = getHook <$> (hooks^.at (typeRep (Proxy :: Proxy a))._Just)

-- | This registers an event listener hook, as long as the listener is well-typed similar to this:
--
-- @MyEventType -> Action ()@ then it will be registered to listen for dispatched events of that type.
-- Use within the 'Rasa.Internal.Scheduler.Scheduler' and add have the user add it to their config.
eventListener :: forall a. Typeable a => (a -> Action ()) -> Scheduler ()
eventListener hook = modify $ insertWith mappend (typeRep (Proxy :: Proxy a)) [Hook hook]

-- | Transform a 'Rasa.Internal.Scheduler.Scheduler' monad into a 'Hooks' map.
getHooks :: Scheduler () -> Hooks
getHooks = flip execState mempty . runSched


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

