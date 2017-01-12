{-# LANGUAGE TemplateHaskell #-}

module Rasa.Ext.Bufs
  ( focusDo
  , getFocus
  , setFocus
  , nextBuf
  , prevBuf
  , bufs
  ) where

import Rasa.Ext

import Control.Lens
import Data.Default
import Data.Typeable

data Focused = Focused
  { _focused' :: Maybe BufRef
  } deriving (Typeable, Show)
makeLenses ''Focused

instance Default Focused where
  def = Focused
    { _focused'=Nothing
    }

bufs :: Scheduler ()
bufs = onBufAdded initFocus
  where
    initFocus bufRef = do
      mBuf <- use focused
      case mBuf of
        Nothing -> setFocus bufRef
        Just _ -> return ()

focused :: HasEditor e => Lens' e (Maybe BufRef)
focused = ext.focused'

getFocus :: Action (Maybe BufRef)
getFocus = use focused

setFocus :: BufRef -> Action ()
setFocus ref = focused ?= ref

focusDo :: BufAction a -> Action (Maybe a)
focusDo bufAct = do
  mFoc <- use focused
  case mFoc of
    Just foc -> bufDo foc bufAct
    Nothing -> return Nothing

-- | Switches focus to the next buffer
nextBuf :: Action ()
nextBuf = do
  mFoc <- use focused
  case mFoc of
    Nothing -> return ()
    Just foc -> do
      next <- nextBufRef foc
      focused ?= next

-- | Switches focus to the previous buffer
prevBuf :: Action ()
prevBuf = do
  mFoc <- use focused
  case mFoc of
    Nothing -> return ()
    Just foc -> do
      prev <- prevBufRef foc
      focused ?= prev
