{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving,
   ExistentialQuantification, ScopedTypeVariables #-}

module Rasa.Scheduler
  ( Scheduler(..)
  , Hooks
  , Hook
  , getHooks
  , eventListener
  , matchingHooks
  , dispatchEvent
  ) where

import Control.Monad.State

import Rasa.Action
import Control.Lens
import Data.Foldable
import Control.Monad.Reader
import Data.Dynamic
import Data.Map
import Unsafe.Coerce

-- | A simple monad that allows registering event listeners
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
-- Use within the 'Rasa.Scheduler.Scheduler' and add have the user add it to their config.
eventListener :: forall a. Typeable a => (a -> Action ()) -> Scheduler ()
eventListener hook = modify $ insertWith mappend (typeRep (Proxy :: Proxy a)) [Hook hook]

-- | Transform a 'Rasa.Scheduler.Scheduler' monad into a 'Hooks' map.
getHooks :: Scheduler () -> Hooks
getHooks = flip execState mempty . runSched
