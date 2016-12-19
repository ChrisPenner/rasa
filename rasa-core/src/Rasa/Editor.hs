{-# LANGUAGE TemplateHaskell, Rank2Types, OverloadedStrings, GADTs #-}
module Rasa.Editor (
    Editor
  , focused
  , buffers
  , exiting
) where

import Control.Lens
import Data.Default (def, Default(..))

import Rasa.Buffer

data Editor = Editor {
    _buffers :: [Buffer]
  , _focused :: Int
  , _exiting :: Bool
} deriving Show

makeLenses ''Editor

instance Default Editor where
    def = Editor {
            _buffers=fmap newBuffer [ "Buffer 0\nHey! How's it going over there?\nI'm having just a splended time!\nAnother line for you sir?"
                                 , "Buffer 1\nHey! How's it going over there?\nI'm having just a splended time!\nAnother line for you sir?" ]
          , _focused=0
          , _exiting=False
             }
