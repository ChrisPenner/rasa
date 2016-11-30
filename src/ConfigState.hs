module ConfigState where

import Ext.Vim.State
import Control.Lens
import Data.Default

data ExtState = ExtState {
                    _vimState :: VimSt
                         }

makeLenses ''ExtState

instance Default ExtState where
    def = ExtState {
    _vimState=def
                   }

instance VimState ExtState where
    vim' = vimState
