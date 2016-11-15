{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module State (
    St
    , buffers
    , focused
    , focusedBuf
    , vHeight
    , mode

  , Mode(..)
  , Buffer
    , buffer
    , text
    , cursor
    , toOffset
    , toCoord
  , Cursor
  , Coord(..)
) where

import Data.Monoid
import Control.Lens
import Data.Default (def, Default(..))
import qualified Data.Text as T

data Mode = Insert | Normal deriving (Show, Eq)

type Cursor = Int
newtype Coord = Coord (Int, Int) deriving (Show, Eq, Ord)

toOffset :: T.Text -> Coord -> Cursor
toOffset txt (Coord (r, c)) = undefined

toCoord :: T.Text -> Cursor -> Coord
toCoord txt n = undefined

data Buffer = Buffer {
    _text :: T.Text
  , _cursor :: Cursor
} deriving (Show)

buffer :: T.Text -> Buffer
buffer t = Buffer {
        _text=t
      , _cursor=0
}

data St = St {
    _buffers :: [Buffer]
  , _focused :: Int
  , _vHeight :: Int
  , _mode :: Mode
} deriving (Show)

instance Default St where
    def = St {
            _buffers=fmap buffer ["Buffer 0", "Buffer 1"]
          , _focused=0
          , _vHeight=10
          , _mode=Normal
             }

makeLenses ''St
makeLenses ''Buffer

focusedBuf :: Lens' St Buffer
focusedBuf = lens getter (flip setter)
    where getter = do
            foc <- view focused
            view (buffers. to (!! foc))

          setter a = do
            foc <- view focused
            set (buffers . ix foc) a
