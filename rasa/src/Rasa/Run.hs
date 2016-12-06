{-# language ExistentialQuantification, Rank2Types #-}
module Rasa.Run (rasa) where

import Rasa.Alteration
import Rasa.Editor

import Control.Lens
import Control.Monad
import Data.Default (def)

handleEvent :: Alteration () -> Store -> IO Store
handleEvent = runAlteration

rasa :: Alteration () -> IO ()
rasa extensions = eventLoop extensions def

eventLoop :: Alteration () -> Store -> IO ()
eventLoop extensions store = do
    newStore <- handleEvent extensions store
    unless (newStore^.editor.exiting) (eventLoop extensions newStore)
