module Main where

import Rasa.Run (rasa)
import Rasa.Ext
import Rasa.Ext.Vim
import Rasa.Adapters.Vty

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

logger :: Alteration ()
logger = do
  evt <- use event
  liftIO $ when (Init `elem` evt) (writeFile "logs.log" "Event Log\n")
  liftIO $ appendFile "logs.log" (show evt ++ "\n")

eventListeners :: [Alteration [Event]]
eventListeners = [vtyEvent]

main :: IO ()
main = rasa eventListeners $ do
  vim
  vty
  -- logger
