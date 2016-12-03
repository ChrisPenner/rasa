{-# language ExistentialQuantification, Rank2Types #-}
module Rasa.Run (rasa) where

import Rasa.Alteration
import Rasa.Editor
import Rasa.Event

import Control.Lens
import Data.Default (def, Default)
import Control.Monad.IO.Class

logStore :: Store e -> IO ()
logStore store = appendFile "logfile" ((++ "\n") . show $ store^.event)

handleEvent :: Alteration e () -> Store e -> IO (Store e)
handleEvent = runAlteration

type Renderer a = forall m. MonadIO m => (m a, a -> Editor -> m (), a -> m Event, a -> m ())

rasa :: Default e => Renderer a -> Alteration e () -> IO ()
rasa renderer@(initUi, _, _, _) extensions = do
    writeFile "logfile" "---\n"
    store <- handleEvent extensions def
    uiState <- initUi
    eventLoop renderer extensions uiState store

eventLoop :: Renderer a -> Alteration e () -> a -> Store e -> IO ()
eventLoop renderer@(_, render, getEvent, shutdown) extensions uiState store = do
    logStore store
    render uiState (store^.editor)
    evt <- getEvent uiState
    let withEvent = store & event .~ [evt]
    newStore <- handleEvent extensions withEvent
    if newStore^.editor.exiting
       then shutdown uiState
       else eventLoop renderer extensions uiState newStore
