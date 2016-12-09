module Rasa.Alteration where

import Control.Monad.State
import Control.Monad.Except

import Rasa.State


type Alteration a = StateT Store (ExceptT String IO) a

execAlteration :: Store -> Alteration () -> IO (Either String Store)
execAlteration store alt = runExceptT $ execStateT alt store

evalAlteration :: Store -> Alteration a -> IO (Either String a)
evalAlteration store alt = runExceptT $ evalStateT alt store
