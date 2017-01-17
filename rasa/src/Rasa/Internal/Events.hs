{-# language ExistentialQuantification #-}
module Rasa.Internal.Events 
  ( Init(..)
  , BeforeEvent(..)
  , BeforeRender(..)
  , OnRender(..)
  , AfterRender(..)
  , Exit(..)
  , BufAdded(..)
  , Keypress(..)
  , Mod(..)
  , BufTextChanged(..)
  ) where

import Data.Dynamic
import Rasa.Internal.Editor
import Rasa.Internal.Range
import qualified Yi.Rope as Y

-- | The Event type represents a common denominator for all actions that could
-- occur Event transmitters express events that have occured as a member of this
-- type. At the moment it's quite sparse, but it will expand as new types of
-- events are needed.

-- | This event is dispatched exactly once when the editor starts up.
data Init = Init deriving (Show, Eq, Typeable)

-- | This event is dispatched immediately before dispatching any events from
-- asyncronous event listeners (like 'Keypress's)
data BeforeEvent = BeforeEvent deriving (Show, Eq, Typeable)

-- | This event is dispatched immediately before dispatching
-- the 'OnRender' event.
data BeforeRender = BeforeRender deriving (Show, Eq, Typeable)

-- | This event is dispatched when it's time for extensions to render to screen.
data OnRender = OnRender deriving (Show, Eq, Typeable)

-- | This event is dispatched immediately after dispatching 'OnRender'.
data AfterRender = AfterRender deriving (Show, Eq, Typeable)

-- | This event is dispatched before exiting the editor, listen for this to do
-- any clean-up (saving files, etc.)
data Exit = Exit deriving (Show, Eq, Typeable)

-- | This event is dispatched after adding a new buffer. The contained BufRef refers to the new buffer.
data BufAdded = BufAdded BufRef deriving (Show, Eq, Typeable)

-- | This event is dispatched in response to keyboard key presses. It contains both
-- the char that was pressed and any modifiers ('Mod') that where held when the key was pressed.
data Keypress
  = Keypress Char
             [Mod]
  | KEsc
  | KBS
  | KEnter
  | KUnknown
  | KLeft
  | KRight
  | KUp
  | KDown
  deriving (Show, Eq, Typeable)

-- | This represents each modifier key that could be pressed along with a key.
data Mod
  = Ctrl
  | Alt
  | Shift
  deriving (Show, Eq)

data BufTextChanged
  = BufTextChanged Range Y.YiString
  deriving (Show, Eq, Typeable)
  
