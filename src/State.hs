{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module State (
    St
  , text
  , buffers
  , focused
  , vHeight
) where

import Data.Monoid
import Control.Lens
import Data.Default (def, Default(..))
import qualified Data.Text as T


data St = St {
    _text :: T.Text
  , _buffers :: [T.Text]
  , _focused :: Int
  , _vHeight :: Int
} deriving (Show)

instance Default St where
    def = St {
            _text=""
          , _buffers= ["Buffer 0", "Buffer 1"]
          , _focused=0
          , _vHeight=10
             }

makeLenses ''St
