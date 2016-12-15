module Rasa.Adapters.Vty.Event (vtyEvent) where

import Rasa.Ext

import Rasa.Adapters.Vty.State
import Control.Monad.IO.Class

import qualified Graphics.Vty as V

vtyEvent :: Action [Event]
vtyEvent = do
    v <- getVty
    liftIO $ ((:[]).convertEvent) <$> V.nextEvent v

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
