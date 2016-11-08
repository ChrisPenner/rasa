{-# LANGUAGE OverloadedStrings #-}
module VtyAdapter (
    convertEvent
    )
    where

import Events(Event(..), Continue(..))
import State

import qualified Graphics.Vty as V
import Data.Text

convertEvent :: V.Event -> Event
convertEvent (V.EvKey e mods) = convertKeypress e mods

convertKeypress :: V.Key -> [V.Modifier] -> Event
convertKeypress V.KEnter _ = Append "\n"
convertKeypress V.KBS _ = Backspace
convertKeypress V.KEsc _ = Exit
convertKeypress (V.KChar w) [V.MCtrl]  = KillWord
convertKeypress (V.KChar c) _  = Append (singleton c)
