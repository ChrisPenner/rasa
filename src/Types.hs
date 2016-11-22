{-# LANGUAGE ExistentialQuantification #-}
module Types where

import Data.Text as T
import Control.Lens

data Mod =
    Ctrl
  | Alt
  | Shift
  deriving (Show)

data Event =
    Keypress Char [Mod]
  | Esc
  | BS
  | Enter
  | Unknown
  deriving (Show)

data Directive =
    Append T.Text
  | DeleteChar
  | KillWord
  | SwitchBuf Int
  | MoveCursor Int
  | MoveCursorCoordBy Coord
  | StartOfLine
  | EndOfLine
  | StartOfBuffer
  | EndOfBuffer
  | FindNext T.Text
  | FindPrev T.Text
  | Noop
  | Exit
  deriving (Show, Eq)

type Offset = Int
type Coord = (Int, Int)
type Size = (Int, Int)

data Buffer c = Buffer {
    _text :: T.Text
  , _cursor :: c
} deriving (Show, Eq)
makeLenses ''Buffer

data Extension = forall extSt. Extension {
    _name :: String
  , _extState :: extSt
  , _apply :: extSt -> St -> Event -> (extSt, [Directive])
}

instance Show Extension where
    show = _name

data St = St {
    _buffers :: [Buffer Offset]
  , _focused :: Int
  , _extensions :: [Extension]
} deriving (Show)
makeLenses ''St

data Continue = Continue St [Directive]
