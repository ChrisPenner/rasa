module Alteration where

import Control.Monad.State

import Event
import State

import Control.Lens

import ConfigState (ExtState)

data Store = Store {
                     _event :: Maybe Event
                   , _editor :: St
                   , _extState :: ExtState
                   }

makeLenses ''Store

type Alteration a = StateT Store IO a

runAlteration :: Alteration () -> Maybe Event -> ExtState -> St -> IO Store
runAlteration alt evt extSt st = execStateT alt store
    where store = Store {
                          _event=evt
                        , _editor=st
                        , _extState=extSt
                       }
