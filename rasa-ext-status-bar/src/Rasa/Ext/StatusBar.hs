{-# LANGUAGE TemplateHaskell #-}

module Rasa.Ext.StatusBar
  ( leftStatus
  , centerStatus
  , rightStatus
  , statusBar
  , StatusBar(..)
  ) where

import Control.Lens

import Data.Typeable
import Data.Default

import qualified Yi.Rope as Y

import Rasa.Ext

data StatusBar = StatusBar
  { _left :: [Y.YiString]
  , _center :: [Y.YiString]
  , _right :: [Y.YiString]
  } deriving (Typeable, Show, Eq)

makeLenses ''StatusBar

instance Default StatusBar where
  def = StatusBar
    { _left=[]
    , _center=[]
    , _right=[]
    }

statusBar :: Action ()
statusBar = beforeEveryEvent_ $ buffersDo_ clearStatus

clearStatus :: BufAction ()
clearStatus = setBufExt $ StatusBar [] [] []

leftStatus :: Y.YiString -> BufAction ()
leftStatus txt = overBufExt (left %~ (txt:))

centerStatus :: Y.YiString -> BufAction ()
centerStatus txt = overBufExt (center %~ (txt:))

rightStatus :: Y.YiString -> BufAction ()
rightStatus txt = overBufExt (right %~ (txt:))
