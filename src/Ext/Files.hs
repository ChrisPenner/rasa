module Ext.Files (files) where

import qualified Data.Text.IO as TIO
import Control.Lens

import Alteration
import Directive
import Buffer
import Event
import Ext.Utils

files :: Alteration ()
files = do
    evt <- getEvent
    mapM_ (apply . perform) evt

perform :: Event -> [Directive]
perform (Keypress 's' [Ctrl]) = [OverBuffer saveFile]
    where saveFile :: Buffer Offset -> IO (Buffer Offset)
          saveFile buf = do
              let fname = buf^.filename
                  contents = buf^.text
              TIO.writeFile fname contents
              return buf

perform _ = []
