{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving,
   ExistentialQuantification, ScopedTypeVariables #-}

module Rasa.Scheduler
  ( Scheduler(..)
  , Hooks
  , Hook
  , getHooks
  , addHook
  , matchingHooks
  ) where

import Control.Monad.State

import Rasa.Action
import Control.Lens
import Data.Dynamic
import Data.Map
import Unsafe.Coerce

data Hook = forall a. Hook a

getHook :: forall a. Hook -> (a -> Action ())
getHook = coerce
  where
    coerce :: Hook -> (a -> Action ())
    coerce (Hook x) = unsafeCoerce x

matchingHooks :: forall a. Typeable a => Hooks -> [a -> Action ()]
matchingHooks hooks = getHook <$> (hooks^.at (typeRep (Proxy :: Proxy a))._Just)

type Hooks = Map TypeRep [Hook]

addHook :: forall a. Typeable a => (a -> Action ()) -> Scheduler ()
addHook hook = modify $ insertWith mappend (typeRep (Proxy :: Proxy a)) [Hook hook]

-- | This is just a writer monad that allows registering Actions into
-- specific lifecycle hooks.
newtype Scheduler a = Scheduler
  { runSched :: State Hooks a
  } deriving (Functor, Applicative, Monad, MonadState Hooks)

getHooks :: Scheduler () -> Hooks
getHooks = flip execState mempty . runSched
