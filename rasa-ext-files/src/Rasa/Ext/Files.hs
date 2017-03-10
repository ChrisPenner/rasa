{-# language
   OverloadedStrings
#-}

module Rasa.Ext.Files
  ( files
  , save
  ) where

import qualified Data.Text.IO as TIO
import System.Environment

import Data.Typeable
import Data.Default
import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import qualified Yi.Rope as Y

import Rasa.Ext
import Rasa.Ext.Views
import Rasa.Ext.Cmd

-- | Stores filename
data FileInfo =
  FileInfo (Maybe String)
  deriving (Typeable, Show, Eq)

instance Default FileInfo where
  def = FileInfo Nothing

type Filename = String

-- | Stores File status; Clean means all changes are saved
data FileStatus =
  Dirty
  | Clean
  deriving Show

instance Default FileStatus where
  def = Clean

-- | Returns 'FileStatus' of current buffer
getFileStatus :: BufAction FileStatus
getFileStatus = getBufExt

-- | Sets 'FileStatus' of current buffer
setFileStatus :: FileStatus -> BufAction ()
setFileStatus = setBufExt

-- | Gets filename of current buffer
getFilename :: BufAction (Maybe Filename)
getFilename = do
  FileInfo filename <- getBufExt
  return filename

-- | Main export, use this in your Rasa config
files :: App ()
files = do
  onEveryNewBuffer_ $ do
    void . onBufTextChanged $ bufferChanged
    void . addTopStatus $ fileStatus
    void . addTopStatus $ (fmap Y.fromString <$> getFilename)

  afterInit $ do
    loadFiles
    addCmd "save" $ focusDo_ . saveAs

-- | Renders the current file status
fileStatus :: BufAction (Maybe RenderInfo)
fileStatus = do
  hasFilename <- isJust <$> getFilename
  status <- getFileStatus
  if hasFilename
    then return . Just $
       case status of
         Dirty -> styleText "✘" $ fg Red
         Clean -> styleText "✓" $ fg Green
    else return Nothing

-- | Keeps track of buffer status
bufferChanged :: BufTextChanged -> BufAction ()
bufferChanged _ = setFileStatus Dirty

saveAs :: String -> BufAction ()
saveAs fName = getText >>= liftIO . TIO.writeFile fName . Y.toText

-- | Save the buffer if we have a filename
save :: BufAction ()
save = do
  FileInfo mName <- getBufExt
  case mName of
    Just fName -> saveAs fName
    Nothing -> return ()
  setFileStatus Clean

-- | Set the filename
setFilename :: String -> BufAction ()
setFilename fname = setBufExt $ FileInfo (Just fname)

-- | Add a buffer for a file
addFile :: String -> Y.YiString -> App ()
addFile fname txt = do
  newBuf <- addBuffer txt
  bufDo_ newBuf (setFilename fname)

-- | Load files from command line
loadFiles :: App ()
loadFiles = do
  fileNames <- liftIO getArgs
  fileTexts <- liftIO $ traverse TIO.readFile fileNames
  mapM_ (uncurry addFile) $ zip fileNames (Y.fromText <$> fileTexts)
