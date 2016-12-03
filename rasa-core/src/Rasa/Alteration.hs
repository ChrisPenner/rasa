{-# LANGUAGE TemplateHaskell #-}
module Rasa.Alteration where

import Control.Monad.State

import Rasa.Event
import Rasa.Editor

import Control.Lens
import Data.Default

data Store extState = Store
  { _event :: [Event]
  , _editor :: Editor
  , _extState :: extState
  }

makeLenses ''Store

instance Default e => Default (Store e) where
  def =
    Store
    { _event = [def]
    , _editor = def
    , _extState = def
    }

type Alteration extState a = StateT (Store extState) IO a

runAlteration :: Alteration e () -> Store e -> IO (Store e)
runAlteration = execStateT
