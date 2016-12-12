{-# LANGUAGE TemplateHaskell, Rank2Types, OverloadedStrings #-}
module Rasa.Editor (
    Editor
  , focusedBuf
  , focused
  , buffers
  , buf
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


focusedBuf :: Lens' Editor Buffer
focusedBuf = lens getter (flip setter)
    where getter = do
            foc <- view focused
            -- TODO use ix here and make it safe??
            view (buffers.to (!! foc))

          setter a = do
            foc <- view focused
            set (buffers . ix foc) a

buf :: Int -> Traversal' Editor Buffer
buf bufN = buffers.ix bufN
