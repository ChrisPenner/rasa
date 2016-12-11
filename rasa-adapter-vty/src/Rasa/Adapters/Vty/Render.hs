{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Adapters.Vty.Render (render') where

import Rasa.Adapter
import Rasa.Ext
import Rasa.Editor
import Rasa.Buffer
import Rasa.View

import Rasa.Adapters.Vty.State
import Control.Monad.IO.Class


import Data.List
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Control.Lens
import Control.Arrow (first)

class Renderable a b where
    render :: Size -> a -> b

instance Renderable Editor V.Image where
    render sz = view $ focusedBuf . to (render sz)

instance Renderable (Buffer Int) V.Image where
    render (width, _) = do
        txt <- textWrap width . view text
        -- curs <- view cursor
        -- return $ applyAttrs [(curs, inverse), (curs + 1, V.defAttr)] txt
        atts <- fmap convertIAttr <$> view attrs
        return $ applyAttrs atts txt
            -- where inverse = V.currentAttr `V.withStyle` V.reverseVideo

applyAttrs :: [(Int, V.Attr)] -> T.Text -> V.Image
applyAttrs atts t = applyAttrs' atts (T.lines t)

convertIAttr :: IAttr -> (Int, V.Attr)
convertIAttr (IAttr i a)= (i, convertAttr a)

convertAttr :: Attr -> V.Attr
convertAttr (Attr (fg', bg', style')) = V.Attr
                                        (maybe V.KeepCurrent convertStyle style')
                                        (maybe V.KeepCurrent convertColor fg')
                                        (maybe V.KeepCurrent convertColor bg')

convertStyle :: Style -> V.MaybeDefault V.Style
convertStyle Standout = V.SetTo V.standout
convertStyle Underline = V.SetTo V.underline
convertStyle ReverseVideo = V.SetTo V.reverseVideo
convertStyle Blink =  V.SetTo V.blink
convertStyle Dim = V.SetTo  V.dim
convertStyle Bold = V.SetTo V.bold
convertStyle DefStyle = V.Default

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

getSize :: Alteration (Int, Int)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v

render' :: Alteration ()
render' = do
  eState <- use editor
  sz <- getSize
  let pic = V.picForImage $ render sz eState
  v <- getVty
  liftIO $ V.update v pic


type AttrList = [(Int, V.Attr)]
decr :: Int -> AttrList -> AttrList
decr n = fmap $ first (subtract n)

plainText :: T.Text -> V.Image
plainText = V.text' V.currentAttr

applyAttrs' :: AttrList -> [T.Text] -> V.Image
applyAttrs' atts lines' = vertCat $ unfoldr attrLines (atts, lines')
  where
    vertCat = foldr (V.<->) V.emptyImage
    attrLines :: (AttrList, [T.Text]) -> Maybe (V.Image, (AttrList, [T.Text]))
    attrLines (_, []) = Nothing
    attrLines (as, l:ls) = let (img, restAs) = attrLine as l
                     in Just (img, (decr 1 restAs, ls))

-- Should be able to clean this up and provide better guarantees if I do a scan
-- over attrs and get each successive mappend of them, then do T.splitAt for
-- each offset, then apply the attr for each section at the beggining of each
-- of T.lines within each group. Ugly I know.
attrLine :: AttrList -> T.Text -> (V.Image, AttrList)
attrLine [] txt = (plainText txt, [])
attrLine ((0, attr):atts) txt = first (V.text' attr "" V.<|>) $ attrLine atts txt
attrLine atts "" = (V.emptyImage, atts)
attrLine allAttrs@((offset, _):_) txt
  -- If the offset is larger, we can add the whole line, then decrement the attr offsets
  | offset > T.length txt = (plainText txt, decr (T.length txt) allAttrs)
  -- The offset occurs within the line, apply it in the middle
  | otherwise = first (prefix V.<|>) suffix
  where prefix = plainText (T.take offset txt)
        suffix = attrLine (decr offset allAttrs) (T.drop offset txt)
