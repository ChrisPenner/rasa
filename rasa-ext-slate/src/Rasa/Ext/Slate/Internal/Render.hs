{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Ext.Slate.Internal.Render (render) where

import Rasa.Ext
import Rasa.Ext.Bufs
import Rasa.Ext.Viewports
import Rasa.Ext.Views
import Rasa.Ext.Style
-- import Rasa.Ext.StatusBar (left, center, right)
import Rasa.Ext.Slate.Internal.State
import Rasa.Ext.Slate.Internal.Attributes
import Control.Monad.IO.Class
import Data.Functor.Foldable

import qualified Graphics.Vty as V
import qualified Yi.Rope as Y
import Control.Lens
import Control.Monad.IO.Class

import Data.Monoid
import Data.Foldable

import qualified Yi.Rope as Y

-- | Given a window size, creates a 'BufAction' which will return an image representing the buffer it's run in.
renderBuf :: (Int, Int) -> BufAction V.Image
renderBuf (width, height) = do
  txt <- use text
  atts <- fmap (fmap convertStyle) <$> use styles
  let img = applyAttrs atts txt
  return $ V.resize width height img
type Width = Int
type Height = Int

-- | Get the current terminal size.
getSize :: Action (Width, Height)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v

-- | Render the Editor
render :: Action ()
render = do
  (width, height) <- getSize
  Views vp <- getViews
  bufs <- collectBuffers
  let img = renderWindow (width, height) $ fmap ((bufs !!) . bufIndex) vp
      pic = V.picForImage img
  v <- getVty
  liftIO $ V.update v pic

-- | Given a window size, creates a 'BufAction' which will return an image representing the buffer it's run in.
collectBuffers :: Action [(Y.YiString, [Span V.Attr])]
collectBuffers = bufDo $ do
  txt <- use rope
  atts <- fmap (fmap convertStyle) <$> use styles
  return [(txt, atts)]

splitByRule :: SplitRule -> Int -> (Int, Int)
splitByRule (Percentage p) sz = (start, end)
  where
    start = ceiling $ fromIntegral sz * p
    end = floor $ fromIntegral sz * (1 - p)

splitByRule (FromStart amt) sz = (start, end)
  where
    start = min sz amt
    end = sz - start

splitByRule (FromEnd amt) sz = (start, end)
  where
    start = sz - end
    end = min sz amt

renderWindow :: (Width, Height) -> Window Split (Y.YiString, [Span V.Attr]) -> V.Image
renderWindow sz win = cata alg (getWin win) sz
  where
    alg (Branch (Split Vert spRule) left right) = \(width, height) ->
      let availWidth = fromIntegral (width - 1)
          (leftWidth, rightWidth) = splitByRule spRule availWidth
          border = V.charFill (V.defAttr `V.withForeColor` V.green) '|' 1 height
       in left (leftWidth, height) V.<|> border V.<|> right (rightWidth, height)

    alg (Branch (Split Hor spRule) top bottom) = \(width, height) ->
      let availHeight = fromIntegral (height - 1)
          (topHeight, bottomHeight) = splitByRule spRule availHeight
          border = V.charFill (V.defAttr `V.withForeColor` V.green) '-' width 1
       in top (width, topHeight) V.<-> border V.<-> bottom (width, bottomHeight)

    alg (Leaf bufInfo) = \(width, height) -> renderView (width, height) bufInfo

renderView :: (Width, Height) -> (Y.YiString, [Span V.Attr]) -> V.Image
renderView (width, height) (txt, atts) = V.resize width height $ applyAttrs atts txt
