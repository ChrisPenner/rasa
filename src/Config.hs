module Config where
import Ext.Vim (vim)
import Ext.Files (files)
import Types

extensions :: [Extension]
extensions = [vim, files]
