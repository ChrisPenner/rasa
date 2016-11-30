module Ext.Vim.State where

import Data.Default (Default, def)
data VimSt = Normal | Insert deriving Show

instance Default VimSt where
    def = Normal
