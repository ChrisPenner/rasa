{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Adapters.Vty.Render (render) where

import Rasa.Ext
import Rasa.Ext.Directive
import Rasa.Ext.StatusBar
import Rasa.Adapters.Vty.State
import Rasa.Adapters.Vty.Attributes
import Control.Monad.IO.Class

import qualified Graphics.Vty as V
import Control.Lens

import qualified Data.Text as T
import Data.List (unfoldr)
import Control.Arrow (second)
import Data.Monoid

renderBuf :: (Int, Int) -> BufAction V.Image
renderBuf (width, height) = do
  txt <- textWrap width <$> use text
  atts <- fmap convertIAttr <$> use attrs
  let img = applyAttrs atts txt
  return $ V.resize width height img

getSize :: Alteration (Int, Int)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v

render :: Alteration ()
render = do
  (width, height) <- getSize
  bufImg <- focusDo $ renderBuf (width, height - 1)
  statusBar <- renderStatus width
  let img = bufImg V.<-> statusBar
      pic = V.picForImage img
  v <- getVty
  liftIO $ V.update v pic

renderStatus :: Int -> Alteration V.Image
renderStatus width = focusDo $ do
  statuses <- use bufExt
  let spacer = T.replicate spacerSize " "
      spacerSize = (width - T.length (T.concat joinedParts)) `div` 2
      parts = [ statuses^.left, statuses^.center, statuses^.right ]
      addSpacer = (<> spacer)
      joinedParts = T.intercalate " | " <$> parts
      fullLine = foldMap addSpacer joinedParts
  return $ V.text' V.defAttr fullLine

textWrap :: Int -> T.Text -> T.Text
textWrap n = T.dropEnd 1 . T.unlines . unfoldr (splitLine n)

splitLine :: Int -> (T.Text -> Maybe (T.Text, T.Text))
splitLine n t
  | T.null t = Nothing
  | T.compareLength (fst . splitAtNewline $ t) n == LT = Just $ splitAtNewline t
  | otherwise = Just $ second (T.append "-> ") $ T.splitAt n t

splitAtNewline :: T.Text -> (T.Text, T.Text)
splitAtNewline = second (T.drop 1) . T.span (/= '\n')
