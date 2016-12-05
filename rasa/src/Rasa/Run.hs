{-# language ExistentialQuantification, Rank2Types #-}
module Rasa.Run (rasa) where

import Rasa.Alteration
import Rasa.Editor

import Control.Lens
import Control.Monad
import Data.Default (def, Default)

handleEvent :: Alteration e () -> Store e -> IO (Store e)
handleEvent = runAlteration

rasa :: Default e => Alteration e () -> IO ()
rasa extensions = eventLoop extensions def

eventLoop :: Alteration e () -> Store e -> IO ()
eventLoop extensions store = do
    newStore <- handleEvent extensions store
    unless (newStore^.editor.exiting) (eventLoop extensions newStore)
