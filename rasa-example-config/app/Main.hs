{-# language OverloadedStrings #-}
module Main where

import Rasa (rasa)
import Rasa.Ext
import Rasa.Ext.Views
import Rasa.Ext.Style
import Rasa.Ext.Vim
import Rasa.Ext.Files
import Rasa.Ext.StatusBar
import Rasa.Ext.Logger
import Rasa.Ext.Cursors
import Rasa.Ext.Slate

import Control.Monad

-- | This is the main of an executable that runs rasa with any extensions the
-- user wants
--
-- The @do@ block is just a normal 'Action' ()

main :: IO ()
main = rasa $ do
  views
  vim
  statusBar
  files
  cursors
  logger
  slate
  style
  onInit . void $ newBuffer "This is a buffer to get you started!\nYou can also pass command line args to rasa"
