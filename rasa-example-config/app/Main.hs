module Main where

import Rasa (rasa)
import Rasa.Ext.Style
import Rasa.Ext.Vim
import Rasa.Ext.Files
import Rasa.Ext.StatusBar
import Rasa.Ext.Logger
import Rasa.Ext.Cursors
import Rasa.Renderer.Slate

-- | This is the main of an executable that runs rasa with any extensions the
-- user wants
--
-- The @do@ block is of type 'Rasa.Ext.Scheduler.Scheduler'

main :: IO ()
main = rasa $ do
  vim
  statusBar
  files
  cursors
  logger
  slate
  style
