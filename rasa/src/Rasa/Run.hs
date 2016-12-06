{-# language ExistentialQuantification, Rank2Types #-}
module Rasa.Run (rasa) where

import Rasa.Alteration
import Rasa.Editor
import Rasa.Event

import Control.Lens
import Control.Concurrent.Async
import Control.Monad
import Data.Default (def)

handleEvent :: Store -> Alteration () -> IO Store
handleEvent = execAlteration

rasa :: [Alteration [Event]] -> Alteration () -> IO ()
rasa eventListeners extensions = eventLoop eventListeners extensions def

isExiting :: Store -> Bool
isExiting = view (editor.exiting)

eventLoop ::  [Alteration [Event]] -> Alteration () -> Store -> IO ()
eventLoop eventListeners extensions store = do
  newStore <- handleEvent store extensions
  unless (isExiting newStore) $ do
    asyncEventListeners <- traverse (async.evalAlteration newStore) eventListeners
    (_, nextEvents) <- waitAny asyncEventListeners
    let withEvent = newStore & event .~ nextEvents
    eventLoop eventListeners extensions withEvent
