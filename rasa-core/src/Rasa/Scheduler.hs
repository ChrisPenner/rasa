{-# LANGUAGE TemplateHaskell, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Rasa.Scheduler
  ( Scheduler(..)
  , onInit
  , beforeEvent
  , onEvent
  , beforeRender
  , onRender
  , onExit
  , afterRender
  , getSchedule
  , runSchedule
  ) where

import Control.Monad.Writer

import Rasa.Alteration
import Data.Default
import Control.Lens

data Schedule = Schedule
  { _onInit :: [Alteration ()]
  , _beforeEvent :: [Alteration ()]
  , _onEvent :: [Alteration ()]
  , _beforeRender :: [Alteration ()]
  , _onRender :: [Alteration ()]
  , _afterRender :: [Alteration ()]
  , _onExit :: [Alteration ()]
  }

makeLenses ''Schedule

instance Monoid Schedule where
  mempty = Schedule mempty mempty mempty mempty mempty mempty mempty
  Schedule a b c d e f g `mappend` Schedule a' b' c' d' e' f' g' =
    Schedule (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g')

instance Default Schedule where
  def = Schedule mempty mempty mempty mempty mempty mempty mempty

newtype Scheduler a = Scheduler
  { runSched :: Writer Schedule a
  } deriving (Functor, Applicative, Monad, MonadWriter Schedule)

getSchedule :: Scheduler () -> Schedule
getSchedule = execWriter . runSched

runSchedule :: Schedule -> Alteration ()
runSchedule s = do
  sequence_ $ s ^. beforeEvent
  sequence_ $ s ^. onEvent
  sequence_ $ s ^. beforeRender
  sequence_ $ s ^. onRender
  sequence_ $ s ^. afterRender
