module Rasa.Renderer.Slate.Event (terminalEvents) where

import Rasa.Ext

import Rasa.Renderer.Slate.State
import Control.Monad.IO.Class
import Data.Maybe

import qualified Graphics.Vty as V

-- | Provides keypress events from the terminal, converted from Vty events.
terminalEvents :: Action [Keypress]
terminalEvents = do
    v <- getVty
    liftIO $ maybeToList . convertEvent <$> V.nextEvent v

-- | Converts a 'V.Event' into a keypress if possible.
convertEvent :: V.Event -> Maybe Keypress
convertEvent (V.EvKey e mods) = convertKeypress e mods
convertEvent _ = Nothing

-- | Converts a 'V.Event' into a keypress if possible.
convertKeypress :: V.Key -> [V.Modifier] -> Maybe Keypress
convertKeypress V.KEnter _ = Just Enter
convertKeypress V.KBS _ = Just BS
convertKeypress V.KEsc _ = Just Esc
convertKeypress (V.KChar c) mods  = Just $ Keypress c (fmap convertMod mods)
convertKeypress _ _  = Nothing

-- | Converts a 'V.Modifier' into a 'Mod'.
convertMod :: V.Modifier -> Mod
convertMod m = case m of
                 V.MShift -> Shift
                 V.MCtrl -> Ctrl
                 V.MMeta -> Alt
                 V.MAlt -> Alt
