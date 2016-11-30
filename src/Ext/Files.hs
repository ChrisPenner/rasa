module Ext.Files (files) where

import qualified Data.Text.IO as TIO
import Control.Lens
import System.Environment

import Data.String (fromString)

import Control.Monad.IO.Class

import Alteration
import Directive
import Buffer
import Event
import Ext.Utils

files :: Alteration ()
files = do
    evt <- getEvent
    mapM_ perform evt

perform :: Event -> Alteration ()
perform (Keypress 's' [Ctrl]) = do
              setEvent Nothing
              buf <- getFocusedBuffer
              let fname = buf^.filename
                  contents = buf^.text
              liftIO $ TIO.writeFile fname contents

perform Init = do
    fileNames <- liftIO getArgs
    files' <- liftIO $ traverse (TIO.readFile . fromString) fileNames
    mapM_ addBuffer $ zip fileNames files'

perform _ = return ()
