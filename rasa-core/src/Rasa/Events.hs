module Rasa.Events where

import Data.Default

data Mod
  = Ctrl
  | Alt
  | Shift
  deriving (Show, Eq)

data Event
  = Keypress Char
             [Mod]
  | Esc
  | BS
  | Enter
  | Init
  | Exit
  | Unknown
  deriving (Show, Eq)

instance Default Event where
  def = Init
