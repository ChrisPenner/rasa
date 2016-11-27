{-# LANGUAGE ExistentialQuantification #-}
module Types where

import qualified Data.Text as T
import Control.Monad.State (State)
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

data CustomOp =
    OverState (St -> St)
  | OverBuffer (Buffer Offset -> Buffer Offset)
  | OverText (T.Text -> T.Text)

instance Eq CustomOp where
    _ == _ = False

instance Show CustomOp where
    show (OverState _) = "OverState"
    show (OverBuffer _) = "OverBuffer"
    show (OverText _) = "OverText"

data Directive =
    CustomOp CustomOp
  | Append T.Text
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
  | DeleteTillEOL
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

data Extension = forall extSt. Extension {
    _name :: String
  , _apply :: St -> Event -> State extSt [Directive]
  , _extState :: extSt
}

instance Show Extension where
    show = _name

data St = St {
    _buffers :: [Buffer Offset]
  , _focused :: Int
  , _extensions :: [Extension]
} deriving (Show)

data Continue = Continue St [Directive]

makeLenses ''Buffer
makeLenses ''St
