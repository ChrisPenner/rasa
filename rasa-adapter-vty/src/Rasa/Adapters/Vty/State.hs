module Rasa.Adapters.Vty.State (getVty) where

import Rasa.Ext
import Control.Lens

import Control.Monad.IO.Class
import qualified Graphics.Vty as V

newtype VtyState = VtyState V.Vty
instance Show VtyState where
  show _ = "VtyState"

initUi :: Action V.Vty
initUi = do
  cfg <- liftIO V.standardIOConfig
  v <- liftIO $ V.mkVty cfg
  ext .= Just (VtyState v)
  return v

getVty :: Action V.Vty
getVty = do
  v <- use ext
  case v of
    Just (VtyState v') -> return v'
    Nothing -> initUi
