module Ext.Files (files) where

import Types
import qualified Data.Text.IO as TIO
import Control.Lens

name :: String
name = "Files"

files :: Extension
files = Extension name applyFiles

applyFiles :: Alteration Extension
applyFiles = do
    evt <- getEvent
    apply $ perform evt
    return files

perform :: Event -> [Directive]
perform (Keypress 's' [Ctrl]) = [OverBuffer saveFile]
    where saveFile :: Buffer Offset -> IO (Buffer Offset)
          saveFile buf = do
              let fname = buf^.filename
                  contents = buf^.text
              TIO.writeFile fname contents
              return buf

perform _ = []
