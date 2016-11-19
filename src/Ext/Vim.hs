module Ext.Vim (toDirective) where

import Control.Lens

import Directives (Directive(..))
import State (St, ExtensionState, getState, extStates, VimSt(..), Mode(..))
import Events (Event(..), Mod(..))
import Data.Default (Default, def)
import qualified Data.Text as T

toDirective :: St -> Event -> (VimSt, [Directive])
toDirective st = fromMode mode
    where (VimSt mode) = getState st

fromMode :: Mode -> Event -> (VimSt, [Directive])
fromMode Insert Esc = (VimSt Normal, [])
fromMode Insert BS = (VimSt Insert, [DeleteChar])
fromMode Insert Enter = (VimSt Insert, [Append "\n"])
fromMode Insert (Keypress 'w' [Ctrl]) = (VimSt Insert, [KillWord])
fromMode Insert (Keypress 'c' [Ctrl]) = (VimSt Insert, [Exit])
fromMode Insert (Keypress c mods) = (VimSt Insert, [Append (T.singleton c)])

fromMode Normal (Keypress 'i' _ )  = (VimSt Insert, [])
fromMode Normal (Keypress 'I' _ )  = (VimSt Insert, [StartOfLine])
fromMode Normal (Keypress 'a' _ )  = (VimSt Insert, [MoveCursor 1])
fromMode Normal (Keypress 'A' _ )  = (VimSt Insert, [EndOfLine])
fromMode Normal (Keypress '0' _ )  = (VimSt Normal, [StartOfLine])
fromMode Normal (Keypress '$' _ )  = (VimSt Normal, [FindNext "\n"])
fromMode Normal (Keypress 'g' _ )  = (VimSt Normal, [StartOfBuffer])
fromMode Normal (Keypress 'G' _ )  = (VimSt Normal, [EndOfBuffer])
fromMode Normal (Keypress 'o' _ )  = (VimSt Insert, [EndOfLine, Append "\n"])
fromMode Normal (Keypress 'O' _ )  = (VimSt Insert, [StartOfLine, Append "\n"])
fromMode Normal (Keypress '+' _ ) = (VimSt Normal, [SwitchBuf 1])
fromMode Normal (Keypress '-' _ ) = (VimSt Normal, [SwitchBuf (-1)])
fromMode Normal (Keypress 'h' _ )  = (VimSt Normal, [MoveCursor (-1)])
fromMode Normal (Keypress 'l' _ )  = (VimSt Normal, [MoveCursor 1])
fromMode Normal (Keypress 'k' _ )  = (VimSt Normal, [MoveCursorCoordBy (-1, 0)])
fromMode Normal (Keypress 'j' _ )  = (VimSt Normal, [MoveCursorCoordBy (1, 0)])
fromMode Normal (Keypress 'f' _ )  = (VimSt Normal, [FindNext "f"])
fromMode Normal (Keypress 'F' _ )  = (VimSt Normal, [FindPrev "f"])
fromMode Normal (Keypress 'X' _) = (VimSt Normal, [DeleteChar])
fromMode Normal (Keypress 'x' _) = (VimSt Normal, [MoveCursor 1, DeleteChar])
fromMode Normal (Keypress 'q' _) = (VimSt Normal, [Exit])
fromMode Normal (Keypress 'c' [Ctrl]) = (VimSt Normal, [Exit])

fromMode mode _ = (VimSt mode, [])
