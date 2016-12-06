module Rasa.Adapters.Vty (vty, vtyEvent) where

import Rasa.Adapters.Vty.Render (render)
import Rasa.Event
import Rasa.Ext

import qualified Graphics.Vty as V
import Control.Monad.IO.Class

getVty :: Alteration V.Vty
getVty = do
  ext <- getPlugin
  case ext of
    Just v -> return v
    Nothing -> initUi

vty :: Alteration ()
vty = do
    evt <- getEvent
    if Exit `elem` evt then shutdown
                       else render'

initUi :: Alteration V.Vty
initUi = do
  cfg <- liftIO V.standardIOConfig
  v <- liftIO $ V.mkVty cfg
  setPlugin v
  return v

getSize :: Alteration (Int, Int)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v

vtyEvent :: Alteration [Event]
vtyEvent = do
    v <- getVty
    liftIO $ ((:[]).convertEvent) <$> V.nextEvent v

shutdown :: Alteration ()
shutdown = do
    v <- getVty
    liftIO $ V.shutdown v

render' :: Alteration ()
render' = do
    editor <- getState
    sz <- getSize
    let pic = V.picForImage $ render sz editor
    v <- getVty
    liftIO $ V.update v pic

convertEvent :: V.Event -> Event
convertEvent (V.EvKey e mods) = convertKeypress e mods
convertEvent _ = Unknown

convertKeypress :: V.Key -> [V.Modifier] -> Event
convertKeypress V.KEnter _ = Enter
convertKeypress V.KBS _ = BS
convertKeypress V.KEsc _ = Esc
convertKeypress (V.KChar c) mods  = Keypress c (fmap convertMod mods)
convertKeypress _ _  = Unknown

convertMod :: V.Modifier -> Mod
convertMod m = case m of
                 V.MShift -> Shift
                 V.MCtrl -> Ctrl
                 V.MMeta -> Alt
                 V.MAlt -> Alt
