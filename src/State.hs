{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module State (
    St
    , buffers
    , focused
    , focusedBuf
    , vHeight
    , mode

  , Mode(..)
    , buffer
  , Offset
  , Coord(..)
) where

import Data.Monoid
import Control.Lens
import Data.Default (def, Default(..))
import qualified Data.Text as T

import Buffer

data Mode = Insert | Normal deriving (Show, Eq)

buffer :: T.Text -> Buffer Offset
buffer t = Buffer {
        _text=t
      , _cursor=0
}

data St = St {
    _buffers :: [Buffer Offset]
  , _focused :: Int
  , _vHeight :: Int
  , _mode :: Mode
} deriving (Show)

instance Default St where
    def = St {
            _buffers=fmap buffer ["Buffer 0\nHey! How's it going over there?\nI'm having just a splended time!\nAnother line for you sir?", "Buffer 1"]
          , _focused=0
          , _vHeight=10
          , _mode=Normal
             }

makeLenses ''St

focusedBuf :: Lens' St (Buffer Offset)
focusedBuf = lens getter (flip setter)
    where getter = do
            foc <- view focused
            view (buffers. to (!! foc))

          setter a = do
            foc <- view focused
            set (buffers . ix foc) a
