module Config where

import ConfigState (ExtState)
import Rasa.Alteration
import Rasa.Ext.Vim (vim)
import Rasa.Adapters.Vty (vty)

extensions :: Alteration ExtState ()
extensions = do
    vty
    vim
