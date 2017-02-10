module Rasa.Ext.Slate.Internal.Event (terminalEvents) where

import Rasa.Ext
import Rasa.Ext.Slate.Internal.State

import Control.Monad

import qualified Graphics.Vty as V

-- | Provides keypress events from the terminal, converted from Vty events.
terminalEvents :: Action ()
terminalEvents = do
    v <- getVty
    asyncActionProvider $ getEvents v
      where
        getEvents v dispatch = forever $ V.nextEvent v >>= dispatch . dispatchKeypress . convertEvent

-- | Converts a 'V.Event' into a keypress if possible.
convertEvent :: V.Event -> Keypress
convertEvent (V.EvKey e mods) = convertKeypress e mods
convertEvent _ = KUnknown

-- | Converts a 'V.Event' into a keypress if possible.
convertKeypress :: V.Key -> [V.Modifier] -> Keypress
convertKeypress V.KEsc mods      = KEsc (fmap convertMod mods)
convertKeypress (V.KChar c) mods = Keypress c (fmap convertMod mods)
convertKeypress V.KBS mods       = KBS (fmap convertMod mods)
convertKeypress V.KEnter mods    = KEnter (fmap convertMod mods)
convertKeypress V.KLeft mods     = KLeft (fmap convertMod mods)
convertKeypress V.KRight mods    = KRight (fmap convertMod mods)
convertKeypress V.KUp mods       = KUp (fmap convertMod mods)
convertKeypress V.KDown mods     = KDown (fmap convertMod mods)
convertKeypress V.KPrtScr mods   = KPrtScr (fmap convertMod mods)
convertKeypress V.KHome mods     = KHome (fmap convertMod mods)
convertKeypress V.KPageUp mods   = KPageUp (fmap convertMod mods)
convertKeypress V.KDel mods      = KDel (fmap convertMod mods)
convertKeypress V.KEnd mods      = KEnd (fmap convertMod mods)
convertKeypress V.KPageDown mods = KPageDown (fmap convertMod mods)
convertKeypress _ _ = KUnknown

-- | Converts a 'V.Modifier' into a 'Mod'.
convertMod :: V.Modifier -> Mod
convertMod m = case m of
                 V.MShift -> Shift
                 V.MCtrl -> Ctrl
                 V.MMeta -> Meta
                 V.MAlt -> Alt
