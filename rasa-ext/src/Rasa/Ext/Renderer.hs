{-# LANGUAGE MultiParamTypeClasses #-}
module Rasa.Ext.Renderer where

import Rasa.Event
import Rasa.Editor

class Renderer m a where
    initUi :: m a
    nextEvent :: a -> m Event
    shutdown :: a ->  m ()
    render :: a -> Editor -> m ()
