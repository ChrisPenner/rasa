{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Adapters.Vty.Render (render') where

import Rasa.Ext
import Rasa.Editor
import Rasa.Buffer
import Rasa.View

import Rasa.Adapters.Vty.State
import Control.Monad.IO.Class

import qualified Graphics.Vty as V
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Lens
import Control.Arrow (first)

class Renderable a b where
    render :: Size -> a -> b

instance Renderable Editor V.Image where
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

getSize :: Alteration (Int, Int)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v

render' :: Alteration ()
render' = do
    editor <- getState
    sz <- getSize
    let pic = V.picForImage $ render sz editor
    v <- getVty
    liftIO $ V.update v pic
