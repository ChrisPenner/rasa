module Rasa.Ext.Files
  ( files
  , saveCurrent
  ) where

import qualified Data.Text.IO as TIO
import Control.Lens
import System.Environment

import Data.String (fromString)

import Control.Monad.IO.Class
import Control.Monad

import Rasa.Ext.Directive
import Rasa.Ext

files :: Alteration e ()
files = do
  evt <- getEvent
  when (Init `elem` evt) loadFiles

saveCurrent :: Alteration e ()
saveCurrent = do
  buf <- getFocusedBuffer
  let fname = buf ^. filename
      contents = buf ^. text
  liftIO $ TIO.writeFile fname contents

loadFiles :: Alteration e ()
loadFiles = do
  fileNames <- liftIO getArgs
  files' <- liftIO $ traverse (TIO.readFile . fromString) fileNames
  mapM_ addBuffer $ zip fileNames files'
