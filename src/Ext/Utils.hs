{-# LANGUAGE Rank2Types #-}

module Ext.Utils where

import Alteration
import Editor
import Event

import Control.Monad.State
import Control.Lens
import ConfigState

getPlugin :: Lens' ExtState a -> Alteration a
getPlugin l = zoom (extState . l) get

setPlugin :: Lens' ExtState a -> a -> Alteration ()
setPlugin l = zoom (extState . l) . put

getState :: Alteration Editor
getState = zoom editor get

getEvent :: Alteration (Maybe Event)
getEvent = zoom event get

setEvent :: Maybe Event -> Alteration ()
setEvent = zoom event . put
