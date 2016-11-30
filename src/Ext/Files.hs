module Ext.Files (files) where

import qualified Data.Text.IO as TIO
import Control.Lens

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
              buf <- getFocusedBuffer
              let fname = buf^.filename
                  contents = buf^.text
              liftIO $ TIO.writeFile fname contents

perform _ = return ()
