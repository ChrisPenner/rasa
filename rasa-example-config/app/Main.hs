module Main where

import Rasa.Run (rasa)
import Rasa.Ext.Style
import Rasa.Ext.Vim
import Rasa.Ext.Files
import Rasa.Ext.StatusBar
import Rasa.Ext.Logger
import Rasa.Ext.Cursors
import Rasa.Renderer.Slate

-- | This is the main of an executable that runs rasa with the given extensions registered to receive event hooks
-- 
-- 'terminalEvents' is an event provider which listens for key-presses etc.
-- The @do@ block is of type 'Rasa.Ext.Scheduler.Scheduler'
main :: IO ()
main = rasa [terminalEvents] $ do
  vim
  statusBar
  files
  cursors
  logger
  slate
  style
