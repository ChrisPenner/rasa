module Ext.Utils where

import Alteration
import Directive
import State
import Event

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Lens

import Ext.Vim.State (VimSt)

apply :: [Directive] -> Alteration ()
apply = tell

getState :: Alteration St
getState = ask

getEvent :: Alteration (Maybe Event)
getEvent = fst <$> get

setEvent :: Maybe Event -> Alteration  ()
setEvent = zoom _1 . put

getVim :: Alteration VimSt
getVim = snd <$> get

setVim :: VimSt -> Alteration ()
setVim = zoom _2 . put
