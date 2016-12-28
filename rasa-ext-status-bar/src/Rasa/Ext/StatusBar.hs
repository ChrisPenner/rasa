{-# LANGUAGE TemplateHaskell #-}

module Rasa.Ext.StatusBar
  ( leftStatus
  , centerStatus
  , rightStatus
  , statusBar
  , left
  , center
  , right
  , StatusBar(..)
  ) where

import Control.Lens

import Data.Typeable
import Data.Default

import qualified Data.Text as T

import Rasa.Ext

data StatusBar = StatusBar
  { _left :: [T.Text]
  , _center :: [T.Text]
  , _right :: [T.Text]
  } deriving (Typeable, Show, Eq)

makeLenses ''StatusBar

instance Default StatusBar where
  def = StatusBar
    { _left=[]
    , _center=[]
    , _right=[]
    }

statusBar :: Scheduler ()
statusBar = beforeEvent $ bufDo clearStatus

clearStatus :: BufAction ()
clearStatus = do
  bufExt.left .= []
  bufExt.center .= []
  bufExt.right .= []

leftStatus :: T.Text -> BufAction ()
leftStatus txt = bufExt.left %= (txt:)

centerStatus :: T.Text -> BufAction ()
centerStatus txt = bufExt.center %= (txt:)

rightStatus :: T.Text -> BufAction ()
rightStatus txt = bufExt.right %= (txt:)
