module Config where

-- import Ext.Vim (vim)
-- import Ext.Files (files)
import ConfigState (ExtState)

import Rasa.Alteration
import Rasa.Ext.Vim

extensions :: Alteration ExtState ()
extensions = vim
  -- files
