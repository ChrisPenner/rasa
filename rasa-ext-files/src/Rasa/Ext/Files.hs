{-# LANGUAGE TemplateHaskell #-}
module Rasa.Ext.Files
  ( files
  , saveCurrent
  ) 
    where

import qualified Data.Text.IO as TIO
import Control.Lens
import System.Environment

import Data.String (fromString)
import Data.Typeable

import Control.Monad
import Control.Monad.IO.Class

import Rasa.Ext.Directive
import Rasa.Ext

data BufFileInfo = BufFileInfo {
_filename :: String
                               }
                               deriving (Typeable, Show, Eq)
makeLenses ''BufFileInfo

files :: Alteration ()
files = do
  evt <- use event
  when (Init `elem` evt) loadFiles

saveCurrent :: Alteration ()
saveCurrent = do
  foc <- use focused
  txt <- preuse $ bufText foc
  fname <- preuse (bufExt foc . _Just . filename)
  liftIO $ sequence $ TIO.writeFile <$> fname <*> txt 
  return ()

setFilename :: (Int, String) -> Alteration ()
setFilename (bufN, fname) =
  bufExt bufN .= (Just $ BufFileInfo fname)

loadFiles :: Alteration ()
loadFiles = do
  fileNames <- liftIO getArgs
  fileTexts <- liftIO $ traverse (TIO.readFile . fromString) fileNames
  mapM_ addBuffer fileTexts
  let pairs = zip ([0.. ]:: [Int]) fileNames
  mapM_ setFilename pairs
