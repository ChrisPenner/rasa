module Ext.Files (files) where

import Types
import qualified Data.Text.IO as TIO
import Control.Lens

files :: Alteration ()
files = do
    evt <- getEvent
    apply $ perform evt
    return ()

perform :: Event -> [Directive]
perform (Keypress 's' [Ctrl]) = [OverBuffer saveFile]
    where saveFile :: Buffer Offset -> IO (Buffer Offset)
          saveFile buf = do
              let fname = buf^.filename
                  contents = buf^.text
              TIO.writeFile fname contents
              return buf

perform _ = []
