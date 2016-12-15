{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rasa.Action where

import Control.Monad.State

import Rasa.Buffer
import Rasa.State

newtype Action a = Action
  { runAlt :: StateT Store IO a
  } deriving (Functor, Applicative, Monad, MonadState Store, MonadIO)

execAction :: Store -> Action () -> IO Store
execAction store alt = execStateT (runAlt alt) store

evalAction :: Store -> Action a -> IO a
evalAction store alt = evalStateT (runAlt alt) store

newtype BufAction a = BufAction
  { getBufAction::StateT Buffer IO a
  } deriving (Functor, Applicative, Monad, MonadState Buffer, MonadIO)

execBufAction :: Buffer -> BufAction a -> IO Buffer
execBufAction buf = flip execStateT buf . getBufAction

runBufAction :: Buffer -> BufAction a -> IO (a, Buffer)
runBufAction buf = flip runStateT buf . getBufAction

