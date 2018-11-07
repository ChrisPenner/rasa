{-# LANGUAGE OverloadedStrings #-}
module Rasa.Ext.Slate.Internal.Attributes where

import Rasa.Ext
import qualified Yi.Rope as Y
import qualified Graphics.Vty as V
import Data.Bifunctor

-- | Convert style from "Rasa.Ext.Style" into 'V.Attr's
convertStyle :: Style -> V.Attr
convertStyle (Style (fg', bg', flair')) = V.Attr
                                        (maybe V.KeepCurrent convertFlair flair')
                                        (maybe V.KeepCurrent convertColor fg')
                                        (maybe V.KeepCurrent convertColor bg')

-- | Convert flair from "Rasa.Ext.Style" into 'V.Style's
convertFlair :: Flair -> V.MaybeDefault V.Style
convertFlair Standout = V.SetTo V.standout
convertFlair Underline = V.SetTo V.underline
convertFlair ReverseVideo = V.SetTo V.reverseVideo
convertFlair Blink =  V.SetTo V.blink
convertFlair Dim = V.SetTo  V.dim
convertFlair Bold = V.SetTo V.bold
convertFlair DefFlair = V.Default

-- | Convert colors from "Rasa.Ext.Style" into 'V.Color's
convertColor :: Color -> V.MaybeDefault V.Color
convertColor Black = V.SetTo V.black
convertColor Red = V.SetTo V.red
convertColor Green = V.SetTo V.green
convertColor Yellow = V.SetTo V.yellow
convertColor Blue = V.SetTo V.blue
convertColor Magenta = V.SetTo V.magenta
convertColor Cyan = V.SetTo V.cyan
convertColor White = V.SetTo V.white
convertColor DefColor = V.Default

-- | helper to reset to default attributes
reset :: V.Image
reset = V.text' V.defAttr ""

-- | A newtype to define a (not necessarily law abiding) Monoid for 'V.Attr' which acts as we like.
newtype AttrMonoid = AttrMonoid {
  getAttr :: V.Attr
}

instance Semigroup AttrMonoid where
  AttrMonoid v <> AttrMonoid v' = AttrMonoid $ v <> v'


-- | We want 'mempty' to be 'V.defAttr' instead of 'V.currentAttr' for use in 'combineSpans'.
instance Monoid AttrMonoid where
  mempty = AttrMonoid V.defAttr

-- | Apply a list of styles to the given text, resulting in a 'V.Image'.
applyAttrs :: RenderInfo -> V.Image
applyAttrs (RenderInfo txt styles) = textAndStylesToImage mergedSpans (padSpaces <$> Y.lines txt)
  where mergedSpans = second getAttr <$> combineSpans (fmap AttrMonoid <$> atts)
        -- Newlines aren't rendered; so we replace them with spaces so they're selectable
        padSpaces = (`Y.append` "  ")
        atts = second convertStyle <$> styles

-- | Makes and image from text and styles
textAndStylesToImage :: [(Coord, V.Attr)] -> [Y.YiString] -> V.Image
textAndStylesToImage atts lines' = V.vertCat $ wrapResets . uncurry attrLine <$> pairLines atts lines'
  where
    wrapResets img = reset V.<|> img V.<|> reset

-- | Applies the list of attrs to the line and returns a 'V.Image'. It assumes that the list
-- contains only 'Coord's on the same line (i.e. row == 0)
--
-- Should be able to clean this up and provide better guarantees if I do a scan
-- over attrs and get each successive mappend of them, then do T.splitAt for
-- each offset, then apply the attr for each section at the begining of each
-- of T.lines within each group. Ugly I know.
attrLine :: [(Coord, V.Attr)] -> Y.YiString -> V.Image
attrLine [] txt = plainText txt
attrLine atts "" = V.text' (mconcat (snd <$> atts)) ""
attrLine ((Coord _ 0, attr):atts) txt = V.text' attr "" V.<|> attrLine atts txt
attrLine atts@((Coord _ col, _):_) txt =
  let (prefix, suffix) = Y.splitAt col txt
   in plainText prefix V.<|> attrLine (decrCol col atts) suffix

-- | Pairs up lines with their styles.
pairLines :: [(Coord, b)] -> [a] -> [([(Coord, b)], a)]
pairLines _ [] = []
pairLines [] ls = zip (repeat []) ls
pairLines crds@((Coord 0 _, _):_) (l:ls) = (takeWhile isSameRow crds, l) : pairLines (decrRow $ dropWhile isSameRow crds) ls
  where isSameRow (Coord 0 _, _) = True
        isSameRow _ = False
pairLines crds (l:ls) = ([], l): pairLines (decrRow crds) ls

-- | Decrements the row of all future attrs' location
decrRow :: [(Coord, a)] -> [(Coord, a)]
decrRow = fmap (\(Coord r c, a) -> (Coord (r-1) c, a))

-- | Decrements the column of all future attrs' location by the given amount
decrCol :: Int -> [(Coord, a)] -> [(Coord, a)]
decrCol n = fmap (\(Coord r c, a) -> (Coord r (c-n), a))

-- | Creates a text image without any new attributes.
plainText :: Y.YiString -> V.Image
plainText = V.text' V.currentAttr . Y.toText
