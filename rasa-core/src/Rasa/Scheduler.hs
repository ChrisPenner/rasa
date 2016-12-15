{-# LANGUAGE TemplateHaskell, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Rasa.Scheduler
  ( Scheduler(..)
  , onInit
  , beforeEvent
  , onEvent
  , beforeRender
  , onRender
  , afterRender
  , onExit
  , getSchedule
  , runSchedule
  ) where

import Control.Monad.Writer

import Rasa.Action
import Data.Default
import Control.Lens

data Schedule = Schedule
  { _onInit :: [Action ()]
  , _beforeEvent :: [Action ()]
  , _onEvent :: [Action ()]
  , _beforeRender :: [Action ()]
  , _onRender :: [Action ()]
  , _afterRender :: [Action ()]
  , _onExit :: [Action ()]
  }

makeLenses ''Schedule

instance Monoid Schedule where
  mempty = Schedule mempty mempty mempty mempty mempty mempty mempty
  Schedule a b c d e f g `mappend` Schedule a' b' c' d' e' f' g' =
    Schedule (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g')

instance Default Schedule where
  def = Schedule mempty mempty mempty mempty mempty mempty mempty

-- | This is just a writer monad that allows registering Actions into
-- specific lifecycle hooks.
newtype Scheduler a = Scheduler
  { runSched :: Writer Schedule a
  } deriving (Functor, Applicative, Monad, MonadWriter Schedule)

getSchedule :: Scheduler () -> Schedule
getSchedule = execWriter . runSched

runSchedule :: Schedule -> Action ()
runSchedule s = do
  sequence_ $ s ^. beforeEvent
  sequence_ $ s ^. onEvent
  sequence_ $ s ^. beforeRender
  sequence_ $ s ^. onRender
  sequence_ $ s ^. afterRender
