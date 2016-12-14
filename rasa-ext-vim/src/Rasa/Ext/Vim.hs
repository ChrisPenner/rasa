{-# Language OverloadedStrings #-}
module Rasa.Ext.Vim
  ( vim
  ) where

import Rasa.Ext
import Rasa.Ext.Files (save)
import Rasa.Ext.Cursors
import Rasa.Ext.Directive
import Rasa.Ext.StatusBar
import Rasa.Ext.Scheduler

import Control.Lens
import Data.Text.Lens (packed)
import Data.Default
import Data.Typeable
import qualified Data.Text as T

data VimSt
  = Normal
  | Insert
  deriving (Show, Typeable)

instance Default VimSt where
  def = Normal

getVim :: BufAction VimSt
getVim = use bufExt

setMode :: VimSt -> BufAction ()
setMode vimst = bufExt .= vimst

vim :: Scheduler ()
vim = do
  onEvent handleEvent
  beforeRender setStatus

handleEvent :: Alteration ()
handleEvent = do
  evt <- use event
  focMode <- focusDo $ do
    mode <- getVim
    case mode of
      Normal -> mapM_ normal evt
      Insert -> mapM_ insert evt
    return mode

  mapM_ (global focMode) evt

setStatus :: Alteration ()
setStatus = focusDo $ do
  mode <- getVim
  centerStatus $ show mode^.packed

global :: VimSt -> Event -> Alteration ()
global Normal (Keypress '+' _) = nextBuf
global Normal (Keypress '-' _) = prevBuf
global _ (Keypress 'c' [Ctrl]) = exit
global _ _ = return ()

insert :: Event -> BufAction ()
insert Esc = setMode Normal
insert BS = moveCursorBy (-1) >> deleteChar
insert Enter = insertText "\n"
-- insert (Keypress 'w' [Ctrl]) = killWord
insert (Keypress c _) = insertText (T.singleton c) >> moveCursorBy 1
insert _ = return ()

normal :: Event -> BufAction ()
normal (Keypress 'i' _) = setMode Insert
normal (Keypress 'I' _) = startOfLine >> setMode Insert
normal (Keypress 'a' _) = moveCursorBy 1 >> setMode Insert
normal (Keypress 'A' _) = endOfLine >> setMode Insert
normal (Keypress '0' _) = startOfLine
normal (Keypress '$' _) = endOfLine
normal (Keypress 'g' _) = moveCursorTo 0

normal (Keypress 'G' _) = do
  txt <- use text
  moveCursorTo $ T.length txt

normal (Keypress 'o' _) = endOfLine >> insertText "\n" >> moveCursorBy 1 >> setMode Insert
normal (Keypress 'O' _) = startOfLine >> insertText "\n" >> setMode Insert
normal (Keypress 'h' _) = moveCursorBy (-1)
normal (Keypress 'l' _) = moveCursorBy 1
normal (Keypress 'k' _) = moveCursorCoord (-1, 0)
normal (Keypress 'j' _) = moveCursorCoord (1, 0)
normal (Keypress 'f' _) = findNext "f"
normal (Keypress 'F' _) = findPrev "f"
normal (Keypress 'X' _) = moveCursorBy (-1) >> deleteChar
normal (Keypress 'x' _) = deleteChar
-- normal (Keypress 'D' _) = deleteTillEOL
normal (Keypress 's' [Ctrl]) = save
normal _ = return ()

endOfLine :: BufAction ()
endOfLine = findNext "\n"

startOfLine :: BufAction ()
startOfLine = findPrev "\n"

