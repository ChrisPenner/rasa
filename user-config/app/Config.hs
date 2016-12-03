module Config where

import ConfigState (ExtState)
import Rasa.Alteration
import Rasa.Ext.Vim (vim)

extensions :: Alteration ExtState ()
extensions = vim
