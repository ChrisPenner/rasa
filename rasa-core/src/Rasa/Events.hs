module Rasa.Events where

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
  | Unknown
  deriving (Show, Eq)
