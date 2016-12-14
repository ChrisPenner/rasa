{-# LANGUAGE TemplateHaskell #-}

module Rasa.Ext.Files
  ( files
  , save
  ) where

import qualified Data.Text.IO as TIO
import Control.Lens
import System.Environment

import Data.String (fromString)
import Data.Typeable
import Data.Default 

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T

import Rasa.Ext.Directive
import Rasa.Ext

data FileInfo = FileInfo
  { _filename :: Maybe String
  } deriving (Typeable, Show, Eq)


makeLenses ''FileInfo

instance Default FileInfo where
  def = FileInfo {
  _filename=Nothing
}

files :: Alteration ()
files = do
  evt <- use event
  when (Init `elem` evt) loadFiles

save :: BufAction ()
save = do
  txt <- use text
  fname <- use $ bufExt.filename
  liftIO $ sequence_ $ TIO.writeFile <$> fname <*> pure txt

setFilename :: String -> BufAction ()
setFilename fname = bufExt .= FileInfo (Just fname)

addFile :: String -> T.Text -> Alteration ()
addFile fname txt = addBufferThen txt (setFilename fname)

loadFiles :: Alteration ()
loadFiles = do
  fileNames <- liftIO getArgs
  fileTexts <- liftIO $ traverse (TIO.readFile . fromString) fileNames
  mapM_ (uncurry addFile) $ zip fileNames fileTexts
