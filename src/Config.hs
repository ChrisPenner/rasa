module Config where
import Ext.Vim (vim, VimSt)
import Ext.Files (files)
import Types

type ExtType = VimSt

runExtensions :: ExtType -> Alteration ExtType
runExtensions vimState = do
    newVimState <- vim vimState
    files
    return newVimState
