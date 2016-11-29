module Ext.Vim (vim, VimSt) where

import Ext.Vim.State
import Directive
import Alteration
import Event

import Ext.Utils

import qualified Data.Text as T

vim :: Alteration ()
vim = do
    mode <- getVim
    let modeFunc = case mode of
                    Normal -> normal
                    Insert -> insert

    evt <- getEvent
    mapM_ (performEvent modeFunc) evt
        where performEvent modeFunc e = 
                let (dirs, newMode) = modeFunc e
                 in apply dirs >> setVim newMode

insert :: Event -> ([Directive], VimSt)
insert Esc = ([], Normal)
insert BS = ([DeleteChar], Insert)
insert Enter = ([Append "\n"], Insert)
insert (Keypress 'w' [Ctrl]) = ([KillWord], Insert)
insert (Keypress 'c' [Ctrl]) = ([Exit], Insert)
insert (Keypress c _) = ([Append $ T.singleton c], Insert)
insert _ = ([], Insert)

normal :: Event -> ([Directive], VimSt)
normal (Keypress 'i' _ )  = ([], Insert)
normal (Keypress 'I' _ )  = ([StartOfLine], Insert)
normal (Keypress 'a' _ )  = ([MoveCursor 1], Insert)
normal (Keypress 'A' _ )  = ([EndOfLine], Insert)
normal (Keypress '0' _ )  = ([StartOfLine], Normal)
normal (Keypress '$' _ )  = ([FindNext "\n"], Normal)
normal (Keypress 'g' _ )  = ([StartOfBuffer], Normal)
normal (Keypress 'G' _ )  = ([EndOfBuffer], Normal)
normal (Keypress 'o' _ )  = ([EndOfLine, Append "\n"], Insert)
normal (Keypress 'O' _ )  = ([StartOfLine, Append "\n"], Insert)
normal (Keypress '+' _ ) = ([SwitchBuf 1], Normal)
normal (Keypress '-' _ ) = ([SwitchBuf (-1)], Normal)
normal (Keypress 'h' _ )  = ([MoveCursor (-1)], Normal)
normal (Keypress 'l' _ )  = ([MoveCursor 1], Normal)
normal (Keypress 'k' _ )  = ([MoveCursorCoordBy (-1, 0)], Normal)
normal (Keypress 'j' _ )  = ([MoveCursorCoordBy (1, 0)], Normal)
normal (Keypress 'f' _ )  = ([FindNext "f"], Normal)
normal (Keypress 'F' _ )  = ([FindPrev "f"], Normal)
normal (Keypress 'X' _) = ([DeleteChar, MoveCursor (-1)], Normal)
normal (Keypress 'x' _) = ([MoveCursor 1, DeleteChar, MoveCursor (-1)], Normal)
normal (Keypress 'D' _ )  = ([DeleteTillEOL], Normal)
normal (Keypress 'q' _) = ([Exit], Normal)
normal (Keypress 'c' [Ctrl]) = ([Exit], Normal)

normal _ = ([], Normal)
