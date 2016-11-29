module Ext.Vim (vim) where

import Types
import qualified Data.Text as T

name :: String
name = "Vim"

vim :: Extension
vim = Extension name (handleEvent normal)

newtype Mode = Mode (Event -> ([Directive], Mode))

handleEvent :: (Event -> ([Directive], Mode)) -> Alteration Extension
handleEvent mode = do
    evt <- getEvent
    let (dirs, Mode newMode) = mode evt
    apply dirs
    return $ Extension name (handleEvent newMode)

insert :: Event -> ([Directive], Mode)
insert Esc = ([], Mode normal)
insert BS = ([DeleteChar], Mode insert)
insert Enter = ([Append "\n"], Mode insert)
insert (Keypress 'w' [Ctrl]) = ([KillWord], Mode insert)
insert (Keypress 'c' [Ctrl]) = ([Exit], Mode insert)
insert (Keypress c _) = ([Append $ T.singleton c], Mode insert)
insert _ = ([], Mode insert)

normal :: Event -> ([Directive], Mode)
normal (Keypress 'i' _ )  = ([], Mode insert)
normal (Keypress 'I' _ )  = ([StartOfLine], Mode insert)
normal (Keypress 'a' _ )  = ([MoveCursor 1], Mode insert)
normal (Keypress 'A' _ )  = ([EndOfLine], Mode insert)
normal (Keypress '0' _ )  = ([StartOfLine], Mode normal)
normal (Keypress '$' _ )  = ([FindNext "\n"], Mode normal)
normal (Keypress 'g' _ )  = ([StartOfBuffer], Mode normal)
normal (Keypress 'G' _ )  = ([EndOfBuffer], Mode normal)
normal (Keypress 'o' _ )  = ([EndOfLine, Append "\n"], Mode insert)
normal (Keypress 'O' _ )  = ([StartOfLine, Append "\n"], Mode insert)
normal (Keypress '+' _ ) = ([SwitchBuf 1], Mode normal)
normal (Keypress '-' _ ) = ([SwitchBuf (-1)], Mode normal)
normal (Keypress 'h' _ )  = ([MoveCursor (-1)], Mode normal)
normal (Keypress 'l' _ )  = ([MoveCursor 1], Mode normal)
normal (Keypress 'k' _ )  = ([MoveCursorCoordBy (-1, 0)], Mode normal)
normal (Keypress 'j' _ )  = ([MoveCursorCoordBy (1, 0)], Mode normal)
normal (Keypress 'f' _ )  = ([FindNext "f"], Mode normal)
normal (Keypress 'F' _ )  = ([FindPrev "f"], Mode normal)
normal (Keypress 'X' _) = ([DeleteChar, MoveCursor (-1)], Mode normal)
normal (Keypress 'x' _) = ([MoveCursor 1, DeleteChar, MoveCursor (-1)], Mode normal)
normal (Keypress 'D' _ )  = ([DeleteTillEOL], Mode normal)
normal (Keypress 'q' _) = ([Exit], Mode normal)
normal (Keypress 'c' [Ctrl]) = ([Exit], Mode normal)

normal _ = ([], Mode normal)
