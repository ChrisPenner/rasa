{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Adapters.Vty.Render (render') where

import Rasa.Ext
import Rasa.Ext.Directive
import Rasa.Adapters.Vty.State
import Rasa.Adapters.Vty.Attributes
import Control.Monad.IO.Class

import qualified Graphics.Vty as V
import Control.Lens

import qualified Data.Text as T
import Data.List (unfoldr)
import Control.Arrow (second)

render :: (Int, Int) -> BufAction V.Image
render (width, _) = do
  txt <- textWrap width <$> use text
  atts <- fmap convertIAttr <$> use attrs
  return $ applyAttrs atts txt

getSize :: Alteration (Int, Int)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v

render' :: Alteration ()
render' = do
  sz <- getSize
  img <- focusDo $ render sz
  let pic = V.picForImage img
  v <- getVty
  liftIO $ V.update v pic

textWrap :: Int -> T.Text -> T.Text
textWrap n = T.dropEnd 1 . T.unlines . unfoldr (splitLine n)

splitLine :: Int -> (T.Text -> Maybe (T.Text, T.Text))
splitLine n t
  | T.null t = Nothing
  | T.compareLength (fst . splitAtNewline $ t) n == LT = Just $ splitAtNewline t
  | otherwise = Just $ second (T.append "-> ") $ T.splitAt n t

splitAtNewline :: T.Text -> (T.Text, T.Text)
splitAtNewline = second (T.drop 1) . T.span (/= '\n')
