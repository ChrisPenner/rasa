module Rasa.Renderer.Slate.State (getVty) where

import Rasa.Ext
import Control.Lens

import Control.Monad.IO.Class
import qualified Graphics.Vty as V

newtype Slate = Slate V.Vty
instance Show Slate where
  show _ = "Slate"

initUi :: Action V.Vty
initUi = do
  cfg <- liftIO V.standardIOConfig
  v <- liftIO $ V.mkVty cfg
  ext .= Just (Slate v)
  return v

getVty :: Action V.Vty
getVty = do
  v <- use ext
  case v of
    Just (Slate v') -> return v'
    Nothing -> initUi
