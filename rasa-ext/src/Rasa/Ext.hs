{-# LANGUAGE Rank2Types #-}

module Rasa.Ext where

import Rasa.Alteration
import Rasa.Editor
import Rasa.Event

import Control.Monad.State
import Control.Lens

getPlugin :: Lens' e a -> Alteration e a
getPlugin l = zoom (extState . l) get

setPlugin :: Lens' e a -> a -> Alteration e ()
setPlugin l = zoom (extState . l) . put

getState :: Alteration e Editor
getState = zoom editor get

getEvent :: Alteration e [Event]
getEvent = zoom event get

setEvent :: [Event] -> Alteration e ()
setEvent = zoom event . put
