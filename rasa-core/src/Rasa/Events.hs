{-# language ExistentialQuantification, StandaloneDeriving #-}
module Rasa.Events where

import Data.Dynamic

-- | The Event type represents a common denominator for all actions that could
-- occur Event transmitters express events that have occured as a member of this
-- type. At the moment it's quite sparse, but it will expand as new types of
-- events are needed.

data Event = forall a. (Show a, Typeable a) => Event a
deriving instance Show Event

data Init = Init deriving (Show, Eq, Typeable)
data BeforeEvent = BeforeEvent deriving (Show, Eq, Typeable)
data OnEvent = OnEvent deriving (Show, Eq, Typeable)
data BeforeRender = BeforeRender deriving (Show, Eq, Typeable)
data OnRender = OnRender deriving (Show, Eq, Typeable)
data AfterRender = AfterRender deriving (Show, Eq, Typeable)
data Exit = Exit deriving (Show, Eq, Typeable)
data Unknown = Unknown deriving (Show, Eq, Typeable)

data Keypress
  = Keypress Char
             [Mod]
  | Esc
  | BS
  | Enter
  deriving (Show, Eq, Typeable)

-- | Mod represents modifier keys that could be pressed along with a key.
data Mod
  = Ctrl
  | Alt
  | Shift
  deriving (Show, Eq)

