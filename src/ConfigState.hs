module ConfigState where

import Ext.Vim.State
import Control.Lens
import Data.Default

data ExtState = ExtState
  {
  _vim' :: VimSt
  }

makeLenses ''ExtState

instance Default ExtState where
  def =
    ExtState
    { _vim' = def
    }
