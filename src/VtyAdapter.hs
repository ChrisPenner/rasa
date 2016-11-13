{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
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
import Data.Monoid ((<>))
import Data.List.Extra (dropEnd)
import Data.List (unfoldr)
import Control.Arrow ((>>>), (&&&))

type Size = (Int, Int)

class Renderable a b where
    render :: Size -> a -> b

convertEvent :: V.Event -> Event
convertEvent (V.EvKey e mods) = convertKeypress e mods
convertEvent _ = Unknown

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

split :: (a -> b) -> (b -> a -> c) -> a -> c
split ab bac a = bac (ab a) a

instance Renderable St V.Image where
    render sz@(width, height) st = head $ fmap (render sz) buffers'
        where buffers' :: [Buffer]
              buffers' = view buffers st


    -- render sz@(width, height) = view buffers
    --                     >>> fmap (view text)
    --                     >>> split
    --                             (div width . length)
    --                             (fmap . textWrap)
    --                     >>> fmap (render sz)
    --                     >>> (fmap.fmap) (V.text' fg)
    --                     >>> split
    --                             (maximum . fmap length)
    --                             (fmap . pad )
    --                     >>> (fmap.fmap) (V.resizeWidth 60)
    --                     >>> foldr1 (zipWith (V.<|>))
    --                     >>> mconcat
    --                     -- >>> fmap (V.resize (width - 1) (height -1))
    --                     -- >>> foldr (V.<|>) V.emptyImage
    --                         where fg = V.withForeColor V.defAttr V.red
    --                               padding :: [V.Image]
    --                               padding = repeat mempty
    --                               pad maxLength l = take maxLength (l ++ padding)


instance Renderable Buffer V.Image where
    render (width, height) = do
        txt <- textWrap width . view text
        return $ (foldMap (V.<->) $ applyAttr (Coord (0, 2)) blue (T.lines txt)) V.emptyImage
            where blue = V.defAttr `V.withForeColor` V.blue

-- type Span = (Cursor, Cursor)

plainText = V.text' V.defAttr
applyAttr :: Cursor -> V.Attr -> [T.Text] -> [V.Image]
applyAttr _ _ [] = []
applyAttr c@(Offset i) attrs t = applyAttr (toCoord (T.unlines t) c) attrs t

applyAttr (Coord (startL, startC)) attrs t
  | startL == 0 && startC == 0 = fmap (V.text' attrs) t
  | startL == 0 = let head' = head t
                      prefix = T.take startC head'
                      suffix = T.drop startC head' : drop 1 t
                      rest = applyAttr (Coord (0, 0)) attrs suffix
                   in (plainText prefix V.<|> head rest) : tail rest
  | otherwise = applyAttr (Coord (0, startC)) attrs (drop startL t)

