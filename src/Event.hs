module Event where

import Data.Default

data Mod
  = Ctrl
  | Alt
  | Shift
  deriving (Show)

data Event
  = Keypress Char
             [Mod]
  | Esc
  | BS
  | Enter
  | Init
  | Unknown
  deriving (Show)

instance Default Event where
  def = Init
