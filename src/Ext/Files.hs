module Ext.Files (files) where

import Types
import Data.Default (def)
import qualified Data.Text.IO as TIO
import Control.Lens

type FileSt = ()

data Mode = Insert
          | Normal
          deriving (Show, Eq)

name :: String
name = "Files"

files :: Extension
files = Extension name applyFiles def

applyFiles :: FileSt -> Alteration Extension
applyFiles _ = do
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
