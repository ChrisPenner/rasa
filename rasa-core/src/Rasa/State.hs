{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Rasa.State where

import Rasa.Event
import Rasa.Buffer
import qualified Rasa.Editor as E

import Data.Dynamic
import Data.Default
import Control.Lens
import Data.Dynamic.Lens
import qualified Data.Text as T

data Store = Store
  { _event :: [Event]
  , _editor :: E.Editor
  , _extState :: [Dynamic]
  }

makeLenses ''Store

instance Default Store where
  def =
    Store
    { _event = [def]
    , _editor = def
    , _extState = []
    }

buffers :: Lens' Store [Buffer Int]
buffers = editor.E.buffers

buf :: Int -> Traversal' Store (Buffer Int)
buf bufN = editor. E.buf bufN

bufText :: Int -> Traversal' Store T.Text
bufText bufN = buf bufN.text

bufExt :: Typeable a => Int -> Traversal' Store a
bufExt bufN = buf bufN.bufExts.traverse._Dynamic

exiting :: Lens' Store Bool
exiting = editor. E.exiting

ext :: Typeable a => Traversal' Store a
ext = extState.traverse._Dynamic

