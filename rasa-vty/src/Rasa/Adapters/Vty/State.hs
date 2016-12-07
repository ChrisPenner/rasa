module Rasa.Adapters.Vty.State (getVty) where

import Rasa.Ext

import Control.Monad.IO.Class
import qualified Graphics.Vty as V

initUi :: Alteration V.Vty
initUi = do
  cfg <- liftIO V.standardIOConfig
  v <- liftIO $ V.mkVty cfg
  setExt v
  return v

getVty :: Alteration V.Vty
getVty = do
  ext <- getExt
  case ext of
    Just v -> return v
    Nothing -> initUi

