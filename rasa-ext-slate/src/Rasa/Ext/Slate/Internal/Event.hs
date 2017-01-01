module Rasa.Ext.Slate.Internal.Event (terminalEvents) where

import Rasa.Ext

import Rasa.Ext.Slate.Internal.State

import qualified Graphics.Vty as V

-- | Provides keypress events from the terminal, converted from Vty events.
terminalEvents :: Action ()
terminalEvents = do
    v <- getVty
    eventProvider $ convertEvent <$> V.nextEvent v

-- | Converts a 'V.Event' into a keypress if possible.
convertEvent :: V.Event -> Keypress
convertEvent (V.EvKey e mods) = convertKeypress e mods
convertEvent _ = Unknown

-- | Converts a 'V.Event' into a keypress if possible.
convertKeypress :: V.Key -> [V.Modifier] -> Keypress
convertKeypress V.KEnter _ = Enter
convertKeypress V.KBS _ = BS
convertKeypress V.KEsc _ = Esc
convertKeypress (V.KChar c) mods  = Keypress c (fmap convertMod mods)
convertKeypress _ _ = Unknown

-- | Converts a 'V.Modifier' into a 'Mod'.
convertMod :: V.Modifier -> Mod
convertMod m = case m of
                 V.MShift -> Shift
                 V.MCtrl -> Ctrl
                 V.MMeta -> Alt
                 V.MAlt -> Alt
