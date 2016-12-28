{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Rasa.Ext.Files
  ( files
  , save
  ) where

import qualified Data.Text.IO as TIO
import Control.Lens
import System.Environment

import Data.Foldable
import Data.Typeable
import Data.Default 
import Data.Monoid

import Control.Monad.IO.Class
import qualified Data.Text as T

import Rasa.Ext
import Rasa.Ext.Cmd
import Rasa.Ext.StatusBar

data FileInfo = FileInfo
  { _filename :: Maybe T.Text
  } deriving (Typeable, Show, Eq)

makeLenses ''FileInfo

instance Default FileInfo where
  def = FileInfo {
  _filename=Nothing
}

files :: Scheduler ()
files = do
  onInit $ do
    loadFiles
    addCmd "save" $ focusDo . saveAs
  beforeRender showFilename

showFilename :: Action ()
showFilename = focusDo $ do
  mName <- use $ bufExt.filename
  traverse_ (leftStatus . disp) mName
      where disp name = "<" <> name <> ">"

saveAs :: T.Text -> BufAction ()
saveAs fName = use text >>= liftIO . TIO.writeFile (T.unpack fName)

save :: BufAction ()
save = do
  mName <- use $ bufExt.filename
  case mName of
    Just fName -> saveAs fName
    Nothing -> return ()

setFilename :: T.Text -> BufAction ()
setFilename fname = bufExt.filename ?= fname

addFile :: T.Text -> T.Text -> Action ()
addFile fname txt = addBufferThen txt (setFilename fname)

loadFiles :: Action ()
loadFiles = do
  fileNames <- liftIO getArgs
  fileTexts <- liftIO $ traverse TIO.readFile fileNames
  mapM_ (uncurry addFile) $ zip (T.pack <$> fileNames) fileTexts
