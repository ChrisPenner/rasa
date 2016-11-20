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

data Buffer c = Buffer {
    _text :: T.Text
  , _cursor :: c
} deriving (Show, Eq)
makeLenses ''Buffer

data St = St {
    _buffers :: [Buffer Offset]
  , _focused :: Int
}
makeLenses ''St

data Extension = forall extSt. Extension {
    _extState :: extSt
  , _apply :: extSt -> St -> Event -> (extSt, [Directive])
}

applyExtension :: St -> Event -> Extension -> (Extension, [Directive])
applyExtension st evt (Extension extSt ap) =
    let (newExtState, dirs) = ap extSt st evt
     in (Extension newExtState ap, dirs)

data Continue = Continue [Extension] [Directive] St
