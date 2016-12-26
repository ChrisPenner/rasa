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

dispatchEvent :: Typeable a => a -> Action ()
dispatchEvent evt = do
  hooks <- ask
  traverse_ ($ evt) (matchingHooks hooks)

getHook :: forall a. Hook -> (a -> Action ())
getHook = coerce
  where
    coerce :: Hook -> (a -> Action ())
    coerce (Hook x) = unsafeCoerce x

matchingHooks :: forall a. Typeable a => Hooks -> [a -> Action ()]
matchingHooks hooks = getHook <$> (hooks^.at (typeRep (Proxy :: Proxy a))._Just)


eventListener :: forall a. Typeable a => (a -> Action ()) -> Scheduler ()
eventListener hook = modify $ insertWith mappend (typeRep (Proxy :: Proxy a)) [Hook hook]

-- | This is just a writer monad that allows registering Actions into
-- specific lifecycle hooks.
newtype Scheduler a = Scheduler
  { runSched :: State Hooks a
  } deriving (Functor, Applicative, Monad, MonadState Hooks)

getHooks :: Scheduler () -> Hooks
getHooks = flip execState mempty . runSched
