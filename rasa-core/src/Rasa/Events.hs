module Rasa.Events where

-- | The Event type represents a common denominator for all actions that could
-- occur Event transmitters express events that have occured as a member of this
-- type. At the moment it's quite sparse, but it will expand as new types of
-- events are needed.

data Event
  = Keypress Char
             [Mod]
  | Esc
  | BS
  | Enter
  | Unknown
  deriving (Show, Eq)

-- | Mod represents modifier keys that could be pressed along with a key.
data Mod
  = Ctrl
  | Alt
  | Shift
  deriving (Show, Eq)

