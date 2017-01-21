module Rasa.Ext.Slate.Internal.Event (terminalEvents) where

import Rasa.Ext
import Rasa.Ext.Slate.Internal.State

import Control.Monad

import qualified Graphics.Vty as V

-- | Provides keypress events from the terminal, converted from Vty events.
terminalEvents :: Action ()
terminalEvents = do
    v <- getVty
    asyncEventProvider $ getEvents v
      where
        getEvents v dispatch = forever $ V.nextEvent v >>= dispatch . convertEvent

-- | Converts a 'V.Event' into a keypress if possible.
convertEvent :: V.Event -> Keypress
convertEvent (V.EvKey e mods) = convertKeypress e mods
convertEvent _ = KUnknown

-- | Converts a 'V.Event' into a keypress if possible.
convertKeypress :: V.Key -> [V.Modifier] -> Keypress
convertKeypress V.KEnter _ = KEnter
convertKeypress V.KBS _ = KBS
convertKeypress V.KEsc _ = KEsc
convertKeypress V.KLeft _ = KLeft
convertKeypress V.KRight _ = KRight
convertKeypress V.KUp _ = KUp
convertKeypress V.KDown _ = KDown
convertKeypress (V.KChar c) mods  = Keypress c (fmap convertMod mods)
convertKeypress _ _ = KUnknown

-- | Converts a 'V.Modifier' into a 'Mod'.
convertMod :: V.Modifier -> Mod
convertMod m = case m of
                 V.MShift -> Shift
                 V.MCtrl -> Ctrl
                 V.MMeta -> Alt
                 V.MAlt -> Alt
