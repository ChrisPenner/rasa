{-# LANGUAGE OverloadedStrings #-}
module VtyAdapter (
    convertEvent
  , render
    )
    where

import Events(Event(..), Mod(..))
import State
import View (textWrap)

import qualified Graphics.Vty as V
import qualified Data.Text as T
import Data.Foldable (fold)
import Control.Lens
import Data.Char
import Data.List.Extra (dropEnd)
import Control.Arrow ((>>>))

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

render :: St -> V.Image
-- render = view buffers
--      >>> fmap (textWrap 40)
--      >>> fmap (V.text' bg)
--      >>> fmap (V.resizeWidth 40)
--      >>> foldr (V.<|>) V.emptyImage
--          where bg = V.withBackColor V.defAttr V.blue 

-- render :: St -> V.Image
-- render = view focusedBuf
--         >>> V.text' fg
--             where fg = V.withForeColor V.defAttr V.red 

render = view buffers
        >>> fmap (V.text' fg)
        >>> fmap (V.resize 40 40)
        >>> fmap (V.pad 2 2 1 4)
        >>> foldr (V.<|>) V.emptyImage
            where fg = V.withForeColor V.defAttr V.red 

     -- >>> over (buffers.mapped) (textWrap 40)
     -- >>> over (buffers.mapped) addCursor
     -- >>> view focusedBuf

