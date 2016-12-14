{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rasa.Alteration where

import Control.Monad.State

import Rasa.Buffer
import Rasa.State

newtype Alteration a = Alteration
  { runAlt :: StateT Store IO a
  } deriving (Functor, Applicative, Monad, MonadState Store, MonadIO)

execAlteration :: Store -> Alteration () -> IO Store
execAlteration store alt = execStateT (runAlt alt) store

evalAlteration :: Store -> Alteration a -> IO a
evalAlteration store alt = evalStateT (runAlt alt) store

newtype BufAction a = BufAction
  { getBufAction::StateT Buffer IO a
  } deriving (Functor, Applicative, Monad, MonadState Buffer, MonadIO)

execBufAction :: Buffer -> BufAction a -> IO Buffer
execBufAction buf = flip execStateT buf . getBufAction

runBufAction :: Buffer -> BufAction a -> IO (a, Buffer)
runBufAction buf = flip runStateT buf . getBufAction
