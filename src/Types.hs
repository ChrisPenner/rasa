{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Types where

import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Lens

type Alteration a = StateT (Maybe Event) (ReaderT St (WriterT [Directive] IO)) a

runAlteration :: Alteration a -> Maybe Event -> St -> IO (a, [Directive])
runAlteration alt evt st = runWriterT $ flip runReaderT st $ evalStateT alt evt

apply :: [Directive] -> Alteration ()
apply = tell

getState :: Alteration St
getState = ask

getEvent :: Alteration (Maybe Event)
getEvent = get

setEvent :: Maybe Event -> Alteration ()
setEvent = put

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
    OverState (St -> IO St)
  | OverBuffer (Buffer Offset -> IO (Buffer Offset))
  | OverText (T.Text -> IO T.Text)
  | Effect (IO ())
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

type Offset = Int
type Coord = (Int, Int)
type Size = (Int, Int)

data Buffer c = Buffer {
    _text :: T.Text
  , _cursor :: c
  , _filename :: String
} deriving (Show, Eq)

data St = St {
    _buffers :: [Buffer Offset]
  , _focused :: Int
}

makeLenses ''Buffer
makeLenses ''St
