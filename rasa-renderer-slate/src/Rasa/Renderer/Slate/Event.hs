module Rasa.Renderer.Slate.Event (slateEvent) where

import Rasa.Ext

import Rasa.Renderer.Slate.State
import Control.Monad.IO.Class
import Data.Maybe

import qualified Graphics.Vty as V

slateEvent :: Action [Keypress]
slateEvent = do
    v <- getVty
    liftIO $ maybeToList . convertEvent <$> V.nextEvent v

convertEvent :: V.Event -> Maybe Keypress
convertEvent (V.EvKey e mods) = convertKeypress e mods
convertEvent _ = Nothing

convertKeypress :: V.Key -> [V.Modifier] -> Maybe Keypress
convertKeypress V.KEnter _ = Just Enter
convertKeypress V.KBS _ = Just BS
convertKeypress V.KEsc _ = Just Esc
convertKeypress (V.KChar c) mods  = Just $ Keypress c (fmap convertMod mods)
convertKeypress _ _  = Nothing

convertMod :: V.Modifier -> Mod
convertMod m = case m of
                 V.MShift -> Shift
                 V.MCtrl -> Ctrl
                 V.MMeta -> Alt
                 V.MAlt -> Alt
