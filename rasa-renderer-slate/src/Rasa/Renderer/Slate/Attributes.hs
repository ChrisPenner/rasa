{-# LANGUAGE OverloadedStrings #-}
module Rasa.Renderer.Slate.Attributes where

import Rasa.Ext
import Rasa.Ext.Style
import qualified Yi.Rope as Y
import qualified Graphics.Vty as V
import Control.Lens

convertStyle :: Style -> V.Attr
convertStyle (Style (fg', bg', flair')) = V.Attr
                                        (maybe V.KeepCurrent convertFlair flair')
                                        (maybe V.KeepCurrent convertColor fg')
                                        (maybe V.KeepCurrent convertColor bg')

convertFlair :: Flair -> V.MaybeDefault V.Style
convertFlair Standout = V.SetTo V.standout
convertFlair Underline = V.SetTo V.underline
convertFlair ReverseVideo = V.SetTo V.reverseVideo
convertFlair Blink =  V.SetTo V.blink
convertFlair Dim = V.SetTo  V.dim
convertFlair Bold = V.SetTo V.bold
convertFlair DefFlair = V.Default

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

reset :: V.Image
reset = V.text' V.defAttr ""

newtype AttrMonoid = AttrMonoid {
  attr' :: V.Attr
}

instance Monoid AttrMonoid where
  mempty = AttrMonoid V.defAttr
  AttrMonoid v `mappend` AttrMonoid v' = AttrMonoid $ v `mappend` v'

applyAttrs :: [Span V.Attr] -> Y.YiString -> V.Image
applyAttrs atts txt = applyAttrs' converted (Y.lines txt)
  where combined = combineSpans (atts & traverse.mapped %~ AttrMonoid)
        converted = combined & traverse._2 %~ attr'

-- | Assumes atts' is sorted
applyAttrs' :: [(Coord, V.Attr)] -> [Y.YiString] -> V.Image
applyAttrs' atts lines' = vertCat $ uncurry attrLine <$> pairLines atts lines'
  where
    vertCat = foldr ((V.<->) . (V.<|> reset)) V.emptyImage

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

pairLines :: [(Coord, b)] -> [a] -> [([(Coord, b)], a)]
pairLines _ [] = []
pairLines [] ls = zip (repeat []) ls
pairLines crds@((Coord 0 _, _):_) (l:ls) = (takeWhile isSameRow crds, l) : pairLines (decrRow $ dropWhile isSameRow crds) ls
  where isSameRow (Coord 0 _, _) = True
        isSameRow _ = False
pairLines crds (l:ls) = ([], l): pairLines (decrRow crds) ls

decrRow :: [(Coord, a)] -> [(Coord, a)]
decrRow = fmap (\(Coord r c, a) -> (Coord (r-1) c, a))

decrCol :: Int -> [(Coord, a)] -> [(Coord, a)]
decrCol n = fmap (\(Coord r c, a) -> (Coord r (c-n), a))

plainText :: Y.YiString -> V.Image
plainText = V.text' V.currentAttr . Y.toText
