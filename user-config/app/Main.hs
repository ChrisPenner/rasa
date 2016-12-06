module Main where

import Rasa.Run (rasa)
import Rasa.Ext
import Rasa.Ext.Vim
import Rasa.Adapters.Vty

import Control.Monad
import Control.Monad.IO.Class

logger :: Alteration ()
logger = do
  evt <- getEvent
  liftIO $ when (Init `elem` evt) (writeFile "logs" "Event Log\n")
  liftIO $ appendFile "logs" (show evt ++ "\n")

main :: IO ()
main = rasa $ do
  vim
  vty
  logger
