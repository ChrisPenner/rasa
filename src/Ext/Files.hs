module Ext.Files (files) where

import Types
import Control.Monad.Reader
import Control.Monad.Writer
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
    (_, evt) <- ask
    tell $ perform evt
    return files

perform :: Event -> [Directive]
perform (Keypress 's' [Ctrl]) = [OverBuffer go]
    where go :: Buffer Offset -> IO (Buffer Offset)
          go buf = do
              let fname = buf^.filename
                  contents = buf^.text
              TIO.writeFile fname contents
              return buf

perform _ = []
