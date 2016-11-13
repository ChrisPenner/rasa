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
  , Cursor(..)
) where

import Data.Monoid
import Control.Lens
import Data.Default (def, Default(..))
import qualified Data.Text as T

data Mode = Insert | Normal deriving (Show)

data Cursor = Offset Int | Coord (Int, Int) deriving (Show)

toOffset :: T.Text -> Cursor -> Cursor
toOffset txt (Coord (r, c)) = undefined
toOffset _ c = c

toCoord :: T.Text -> Cursor -> Cursor
toCoord txt (Offset n) = undefined
toCoord _ c = c

data Buffer = Buffer {
    _text :: T.Text
  , _cursor :: Cursor
} deriving (Show)

buffer :: T.Text -> Buffer
buffer t = Buffer {
        _text=t
      , _cursor=Offset 0
}

data St = St {
    _buffers :: [Buffer]
  , _focused :: Int
  , _vHeight :: Int
  , _mode :: Mode
} deriving (Show)

instance Default St where
    def = St {
            _buffers=fmap buffer ["Buffer 0"]
          , _focused=0
          , _vHeight=10
          , _mode=Insert
             }

makeLenses ''St
makeLenses ''Buffer

focusedBuf :: Lens' St Buffer
focusedBuf = lens getter (flip setter)
    where getter = do
            foc <- view focused
            (!! foc) . view buffers

          setter a = do
            foc <- view focused
            set (buffers . ix foc) a

