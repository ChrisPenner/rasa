{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module VtyAdapter (
    convertEvent
  , render
    )
    where

import State
import Buffer
import View
import Event

import qualified Graphics.Vty as V
import qualified Data.Text as T
import Control.Lens
import Data.Monoid ((<>))
import Control.Arrow (first)

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

instance Renderable St V.Image where
    render sz = view $ focusedBuf . to (render sz)

instance Renderable (Buffer Offset) V.Image where
    render (width, _) = do
        txt <- textWrap width . view text
        curs <- view cursor
        coord <- view $ asCoord.cursor
        let styled = applyAttrs [(curs, inverse), (curs + 1, V.defAttr)] txt
            txtLines = foldMap (V.<->) styled V.emptyImage
            cursorLine = V.text' V.defAttr (T.pack $ show curs <> " | " <> show coord)

        return $ cursorLine V.<-> txtLines

            where inverse = V.currentAttr `V.withStyle` V.reverseVideo

applyAttrs :: [(Offset, V.Attr)] -> T.Text -> [V.Image]
applyAttrs attrs t = applyAttrs' attrs (T.lines t)

applyAttrs' :: [(Offset, V.Attr)] -> [T.Text] -> [V.Image]
applyAttrs' _ [] = []
applyAttrs' [] lines' = fmap (V.text' V.currentAttr) lines'
applyAttrs' allAttrs@((offset, attr):attrs) (l:lines')
  | T.length l < offset = plainText l : applyAttrs' (decr (T.length l + 1) allAttrs) lines'
  | otherwise = let prefix = plainText (T.take offset l) V.<|> V.text' attr ""
                    suffix = if null rest
                                then V.emptyImage
                                else head rest
                    rest = applyAttrs' (decr offset attrs) (T.drop offset l:lines')
                 in (prefix V.<|> suffix) : drop 1 rest

decr :: Int ->  [(Offset, V.Attr)] -> [(Offset, V.Attr)]
decr n = fmap $ first (subtract n)

plainText :: T.Text -> V.Image
plainText = V.text' V.currentAttr
