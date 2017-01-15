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
import qualified Yi.Rope as Y

import Rasa.Ext
import Rasa.Ext.Views
import Rasa.Ext.Cmd
import Rasa.Ext.StatusBar

data FileInfo = FileInfo
  { _filename :: Maybe Y.YiString
  } deriving (Typeable, Show, Eq)

makeLenses ''FileInfo

instance Default FileInfo where
  def = FileInfo {
  _filename=Nothing
}

files :: Action ()
files = do 
  beforeEveryRender_ showFilename
  onInit $ do
    loadFiles
    addCmd "save" $ focusDo_ . saveAs . Y.fromString

showFilename :: Action ()
showFilename = focusDo_ $ do
  mName <- use $ bufExt.filename
  traverse_ (leftStatus . disp) mName
      where disp name = "<" <> name <> ">"

saveAs :: Y.YiString -> BufAction ()
saveAs fName = use text >>= liftIO . TIO.writeFile (Y.toString fName) . Y.toText

save :: BufAction ()
save = do
  mName <- use $ bufExt.filename
  case mName of
    Just fName -> saveAs fName
    Nothing -> return ()

setFilename :: Y.YiString -> BufAction ()
setFilename fname = bufExt.filename ?= fname

addFile :: Y.YiString -> Y.YiString -> Action ()
addFile fname txt = do
  newBuf <- newBuffer txt
  bufDo_ newBuf (setFilename fname)

loadFiles :: Action ()
loadFiles = do
  fileNames <- liftIO getArgs
  fileTexts <- liftIO $ traverse TIO.readFile fileNames
  mapM_ (uncurry addFile) $ zip (Y.fromString <$> fileNames) (Y.fromText <$> fileTexts)
