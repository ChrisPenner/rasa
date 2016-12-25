{-# LANGUAGE TemplateHaskell, DeriveFunctor, GeneralizedNewtypeDeriving,
   ExistentialQuantification, ScopedTypeVariables, FlexibleInstances #-}

-- {-# LANGUAGE Rank2Types, TemplateHaskell, OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   -- GeneralizedNewtypeDeriving, FlexibleInstances,
   -- StandaloneDeriving #-}

module Rasa.Scheduler
  ( Scheduler(..)
  , Hooks
  , Hook
  , getHooks
  , addHook
  , getHook
  ) where

import Control.Monad.State

import Rasa.Action
import Control.Lens
import Data.Dynamic
import Data.Map
import Unsafe.Coerce

data Hook = forall a. Hook a

getHook :: forall a. Hook -> a
getHook = coerce 
  where
    coerce :: Hook -> a
    coerce (Hook x) = unsafeCoerce x

type Hooks = Map TypeRep Hook

addHook :: forall a. Typeable a => (a -> Action ()) -> Scheduler ()
addHook hook = at (typeRep (Proxy :: Proxy a)) ?= Hook hook

-- | This is just a writer monad that allows registering Actions into
-- specific lifecycle hooks.
newtype Scheduler a = Scheduler
  { runSched :: State Hooks a
  } deriving (Functor, Applicative, Monad, MonadState Hooks)

getHooks :: Scheduler () -> Hooks
getHooks = flip execState mempty . runSched
