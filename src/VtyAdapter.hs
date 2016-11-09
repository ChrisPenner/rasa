{-# LANGUAGE OverloadedStrings #-}
module VtyAdapter (
    convertEvent
    )
    where

import Events(Event(..), Mod(..))
import State

import qualified Graphics.Vty as V
import qualified Data.Text as T
import Control.Lens
import Data.Char
import Data.List.Extra (dropEnd)

convertEvent :: V.Event -> Event
convertEvent (V.EvKey e mods) = convertKeypress e mods

convertKeypress :: V.Key -> [V.Modifier] -> Event
convertKeypress V.KEnter _ = Enter
convertKeypress V.KBS _ = BS
convertKeypress V.KEsc _ = Esc
convertKeypress (V.KChar c) mods  = Keypress c (fmap convertMod mods)

convertMod :: V.Modifier -> Mod
convertMod m = case m of
                 V.MShift -> Shift
                 V.MCtrl -> Ctrl
                 V.MMeta -> Alt
                 V.MAlt -> Alt
