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

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T

import Rasa.Ext.Directive
import Rasa.Ext

data BufFileInfo = BufFileInfo
  { _filename :: String
  } deriving (Typeable, Show, Eq)

makeLenses ''BufFileInfo

files :: Alteration ()
files = do
  evt <- use event
  when (Init `elem` evt) loadFiles

save :: BufAction ()
save = do
  txt <- use text
  fname <- preuse $ bufExt . _Just . filename
  liftIO $ sequence_ $ TIO.writeFile <$> fname <*> pure txt

setFilename :: String -> BufAction ()
setFilename fname = bufExt .= (Just $ BufFileInfo fname)

addFile :: String -> T.Text -> Alteration ()
addFile fname txt = addBufferThen txt (setFilename fname)

loadFiles :: Alteration ()
loadFiles = do
  fileNames <- liftIO getArgs
  fileTexts <- liftIO $ traverse (TIO.readFile . fromString) fileNames
  mapM_ (uncurry addFile) $ zip fileNames fileTexts
