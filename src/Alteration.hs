module Alteration where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Event
import State
import Directive

import ConfigState (ExtState)

type Alteration a = StateT (Maybe Event, ExtState) (ReaderT St (WriterT [Directive] IO)) a

runAlteration :: Alteration () -> Maybe Event -> ExtState -> St -> IO (ExtState, [Directive])
runAlteration alt evt extState st = ignoreEvent <$> runWriterT (flip runReaderT st $ execStateT alt (evt, extState))
    where ignoreEvent ((_, extSt), dirs) = (extSt, dirs)
