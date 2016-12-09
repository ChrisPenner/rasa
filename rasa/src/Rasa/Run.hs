{-# language ExistentialQuantification, Rank2Types #-}
module Rasa.Run (rasa) where

import Rasa.Ext
import Rasa.State
import Rasa.Alteration

import Control.Lens
import Control.Concurrent.Async
import Control.Monad
import Data.Default (def)

log' :: String -> IO ()
log' msg = appendFile "logs.log" (msg ++ "\n")

handleEvent :: Store -> Alteration () -> IO (Either String Store)
handleEvent = execAlteration

rasa :: [Alteration [Event]] -> Alteration () -> IO ()
rasa eventListeners extensions = eventLoop eventListeners extensions def

eventLoop ::  [Alteration [Event]] -> Alteration () -> Store -> IO ()
eventLoop eventListeners extensions store = do
  res <- handleEvent store extensions
  newStore <- case res of
                Left err -> log' err >> return store
                Right nStore -> return nStore

  unless (newStore^.exiting) $ do
    asyncEventListeners <- traverse (async.evalAlteration newStore) eventListeners
    (_, asyncRes) <- waitAny asyncEventListeners
    withEvents <- case asyncRes of
                    Left err -> log' err >> return newStore
                    Right nextEvents -> return (newStore & event .~ nextEvents)
    eventLoop eventListeners extensions withEvents
