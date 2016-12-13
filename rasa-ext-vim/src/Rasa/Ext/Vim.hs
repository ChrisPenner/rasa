{-# Language OverloadedStrings #-}
module Rasa.Ext.Vim
  ( vim
  ) where

import Rasa.Ext
import Rasa.Ext.Files (saveCurrent)
import Rasa.Ext.Cursors
import Rasa.Ext.Directive

import Control.Lens
import Control.Monad.State
import Data.Default
import Data.Typeable
import qualified Data.Text as T

data VimSt
  = Normal
  | Insert
  deriving (Show, Typeable)

instance Default VimSt where
  def = Normal

getVim :: Alteration VimSt
getVim = do
  mode <- use ext
  case mode of
    Just m -> return m
    Nothing -> do
      ext .= Just (def :: VimSt)
      return def

setMode :: VimSt -> Alteration ()
setMode vimst = ext .= Just vimst

vim :: Alteration ()
vim = do
  mode <- getVim
  let modeFunc =
        case mode of
          Normal -> normal
          Insert -> insert
  evt <- use event
  mapM_ modeFunc evt

insert :: Event -> Alteration ()
insert Esc = ext ?= Normal
insert BS = withFocus (moveCursorBy (-1)) >> withFocus deleteChar
insert Enter = insertText "\n"
-- insert (Keypress 'w' [Ctrl]) = killWord
insert (Keypress 'c' [Ctrl]) = exit
insert (Keypress c _) = insertText (T.singleton c) >> withFocus (moveCursorBy 1)
insert _ = return ()

normal :: Event -> Alteration ()
normal (Keypress 'i' _) = setMode Insert
normal (Keypress 'I' _) = startOfLine >> setMode Insert
normal (Keypress 'a' _) = withFocus (moveCursorBy 1) >> setMode Insert
normal (Keypress 'A' _) = endOfLine >> setMode Insert
normal (Keypress '0' _) = startOfLine
normal (Keypress '$' _) = endOfLine
normal (Keypress 'g' _) = withFocus (moveCursorTo 0)

normal (Keypress 'G' _) = do
  foc <- use focused
  txt <- get <&> (^?!bufText foc)
  withFocus (moveCursorTo $ T.length txt)

normal (Keypress 'o' _) = endOfLine >> insertText "\n" >> withFocus (moveCursorBy 1) >> setMode Insert
normal (Keypress 'O' _) = startOfLine >> insertText "\n" >> setMode Insert
-- normal (Keypress '+' _) = switchBuf 1
-- normal (Keypress '-' _) = switchBuf (-1)
normal (Keypress 'h' _) = withFocus $ moveCursorBy (-1)
normal (Keypress 'l' _) = withFocus $ moveCursorBy 1
normal (Keypress 'k' _) = withFocus $ moveCursorCoord (-1, 0)
normal (Keypress 'j' _) = withFocus $ moveCursorCoord (1, 0)
normal (Keypress 'f' _) = withFocus $ findNext "f"
normal (Keypress 'F' _) = withFocus $ findPrev "f"
normal (Keypress 'X' _) = withFocus (moveCursorBy (-1)) >> withFocus deleteChar
normal (Keypress 'x' _) = withFocus deleteChar
-- normal (Keypress 'D' _) = deleteTillEOL
normal (Keypress 'q' _) = exit
normal (Keypress 'c' [Ctrl]) = exit
normal (Keypress 's' [Ctrl]) = saveCurrent
normal _ = return ()

endOfLine :: Alteration ()
endOfLine = withFocus (findNext "\n")

startOfLine :: Alteration ()
startOfLine = withFocus (findPrev "\n")

