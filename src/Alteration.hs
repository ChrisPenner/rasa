module Alteration where

import Control.Monad.State

import Event
import Editor

import Control.Lens
import Data.Default

import ConfigState (ExtState)

data Store = Store {
                     _event :: Maybe Event
                   , _editor :: Editor
                   , _extState :: ExtState
                   }

makeLenses ''Store

instance Default Store where
    def = Store {
    _event=Just def
  , _editor=def
  , _extState=def
                }

type Alteration a = StateT Store IO a

runAlteration :: Alteration () -> Store -> IO Store
runAlteration = execStateT
