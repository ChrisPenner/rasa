{-# language
   OverloadedStrings
#-}

module Rasa.Ext.Files
  ( files
  , save
  ) where

import qualified Data.Text.IO as TIO
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

data FileInfo =
  FileInfo (Maybe Y.YiString)
  deriving (Typeable, Show, Eq)

instance Default FileInfo where
  def = FileInfo Nothing

files :: Action ()
files = do
  beforeEveryRender_ showFilename
  afterInit $ do
    loadFiles
    addCmd "save" $ focusDo_ . saveAs . Y.fromString

showFilename :: Action ()
showFilename = focusDo_ $ do
  FileInfo mName <- getBufExt
  traverse_ (leftStatus . disp) mName
      where disp name = "<" <> name <> ">"

saveAs :: Y.YiString -> BufAction ()
saveAs fName = getText >>= liftIO . TIO.writeFile (Y.toString fName) . Y.toText

save :: BufAction ()
save = do
  FileInfo mName <- getBufExt
  case mName of
    Just fName -> saveAs fName
    Nothing -> return ()

setFilename :: Y.YiString -> BufAction ()
setFilename fname = setBufExt $ FileInfo (Just fname)

addFile :: Y.YiString -> Y.YiString -> Action ()
addFile fname txt = do
  newBuf <- addBuffer txt
  bufDo_ newBuf (setFilename fname)

loadFiles :: Action ()
loadFiles = do
  fileNames <- liftIO getArgs
  fileTexts <- liftIO $ traverse TIO.readFile fileNames
  mapM_ (uncurry addFile) $ zip (Y.fromString <$> fileNames) (Y.fromText <$> fileTexts)
