module Config where
import Ext.Vim (vim)
import Ext.Files (files)

import Alteration

runExtensions :: Alteration ()
runExtensions = do
    vim
    files
