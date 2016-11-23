module Ext.Vim (vim) where

import Types
import Control.Monad.State
import Data.Default (Default, def)
import qualified Data.Text as T

data VimSt =
    VimSt Mode
    deriving (Show, Eq)

data Mode = Insert
          | Normal
          deriving (Show, Eq)

instance Default VimSt where
    def = VimSt Normal

vim :: Extension
vim = Extension "Vim" applyVim def

applyVim :: St -> Event -> State VimSt [Directive]
applyVim _ evt = state go
    where go (VimSt mode) = fromMode mode evt

fromMode :: Mode -> Event -> ([Directive], VimSt)
fromMode Insert Esc = ([], VimSt Normal)
fromMode Insert BS = ([DeleteChar], VimSt Insert)
fromMode Insert Enter = ([Append "\n"], VimSt Insert)
fromMode Insert (Keypress 'w' [Ctrl]) = ([KillWord], VimSt Insert)
fromMode Insert (Keypress 'c' [Ctrl]) = ([Exit], VimSt Insert)
fromMode Insert (Keypress c _) = ([Append $ T.singleton c], VimSt Insert)

fromMode Normal (Keypress 'i' _ )  = ([], VimSt Insert)
fromMode Normal (Keypress 'I' _ )  = ([StartOfLine], VimSt Insert)
fromMode Normal (Keypress 'a' _ )  = ([MoveCursor 1], VimSt Insert)
fromMode Normal (Keypress 'A' _ )  = ([EndOfLine], VimSt Insert)
fromMode Normal (Keypress '0' _ )  = ([StartOfLine], VimSt Normal)
fromMode Normal (Keypress '$' _ )  = ([FindNext "\n"], VimSt Normal)
fromMode Normal (Keypress 'g' _ )  = ([StartOfBuffer], VimSt Normal)
fromMode Normal (Keypress 'G' _ )  = ([EndOfBuffer], VimSt Normal)
fromMode Normal (Keypress 'o' _ )  = ([EndOfLine, Append "\n"], VimSt Insert)
fromMode Normal (Keypress 'O' _ )  = ([StartOfLine, Append "\n"], VimSt Insert)
fromMode Normal (Keypress '+' _ ) = ([SwitchBuf 1], VimSt Normal)
fromMode Normal (Keypress '-' _ ) = ([SwitchBuf (-1)], VimSt Normal)
fromMode Normal (Keypress 'h' _ )  = ([MoveCursor (-1)], VimSt Normal)
fromMode Normal (Keypress 'l' _ )  = ([MoveCursor 1], VimSt Normal)
fromMode Normal (Keypress 'k' _ )  = ([MoveCursorCoordBy (-1, 0)], VimSt Normal)
fromMode Normal (Keypress 'j' _ )  = ([MoveCursorCoordBy (1, 0)], VimSt Normal)
fromMode Normal (Keypress 'f' _ )  = ([FindNext "f"], VimSt Normal)
fromMode Normal (Keypress 'F' _ )  = ([FindPrev "f"], VimSt Normal)
fromMode Normal (Keypress 'X' _) = ([DeleteChar], VimSt Normal)
fromMode Normal (Keypress 'x' _) = ([MoveCursor 1, DeleteChar], VimSt Normal)
fromMode Normal (Keypress 'D' _ )  = ([DeleteTillEOL], VimSt Normal)
fromMode Normal (Keypress 'q' _) = ([Exit], VimSt Normal)
fromMode Normal (Keypress 'c' [Ctrl]) = ([Exit], VimSt Normal)

fromMode mode _ = ([], VimSt mode)
