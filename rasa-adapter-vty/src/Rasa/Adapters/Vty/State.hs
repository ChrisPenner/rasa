module Rasa.Adapters.Vty.State (getVty) where

import Rasa.Ext
import Control.Lens

import Control.Monad.IO.Class
import qualified Graphics.Vty as V

initUi :: Alteration V.Vty
initUi = do
  cfg <- liftIO V.standardIOConfig
  v <- liftIO $ V.mkVty cfg
  ext .= Just v
  return v

getVty :: Alteration V.Vty
getVty = do
  v <- use ext
  case v of
    Just v' -> return v'
    Nothing -> initUi
