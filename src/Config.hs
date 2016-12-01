module Config where

import Ext.Vim (vim)
import Ext.Files (files)

import Alteration

extensions :: Alteration ()
extensions = do
  files
  vim
