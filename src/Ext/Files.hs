module Ext.Files (files) where

import Types
import Control.Monad.State
import Data.Default (Default, def)
import qualified Data.Text.IO as TIO
import Control.Lens

data FileSt = FileSt
    deriving (Show, Eq)

data Mode = Insert
          | Normal
          deriving (Show, Eq)

instance Default FileSt where
    def = FileSt

files :: Extension
files = Extension "Files" applyFiles def

applyFiles :: St -> Event -> State FileSt [Directive]
applyFiles _ evt = state $ perform evt

perform :: Event -> FileSt -> ([Directive], FileSt)
perform (Keypress 's' [Ctrl]) fileSt = ([OverBuffer go], fileSt)
    where go :: Buffer Offset -> IO (Buffer Offset)
          go buf = do
              let fname = buf^.filename
                  contents = buf^.text
              TIO.writeFile fname contents
              return buf

perform _ fileSt = ([], fileSt)
