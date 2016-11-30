module Ext.Utils where

import Alteration
import State
import Event

import Control.Monad.State
import Control.Lens

import Ext.Vim.State (VimSt)

getState :: Alteration St
getState = zoom editor get

getEvent :: Alteration (Maybe Event)
getEvent = zoom event get

setEvent :: Maybe Event -> Alteration  ()
setEvent = zoom event . put

getVim :: Alteration VimSt
getVim = zoom extState get

setVim :: VimSt -> Alteration ()
setVim = zoom extState . put
