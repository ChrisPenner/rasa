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
import Control.Lens
import Control.Monad.IO.Class

import Data.Monoid
import Data.Foldable

import qualified Yi.Rope as Y
import Control.Monad.State

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
  let img = renderWindow (width, height) $ fmap (fmap (bufs !!)) vp
      pic = V.picForImage img
  v <- getVty
  liftIO $ V.update v pic

-- | Given a window size, creates a 'BufAction' which will return an image representing the buffer it's run in.
collectBuffers :: Action [Buffer]
collectBuffers = bufDo $ do
  buf <- get
  return [buf]

splitByRule :: SplitRule -> Int -> (Int, Int)
splitByRule (Ratio p) sz = (start, end)
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

renderWindow :: (Width, Height) -> BiTree Split (View Buffer) -> V.Image
renderWindow sz win = cata alg win sz
  where
    alg (BranchF (Split Vert spRule) left right) = \(width, height) ->
      let availWidth = fromIntegral (width - 1)
          (leftWidth, rightWidth) = splitByRule spRule availWidth
          border = V.charFill (V.defAttr `V.withForeColor` V.green) '|' 1 height
       in left (leftWidth, height) V.<|> border V.<|> right (rightWidth, height)

    alg (BranchF (Split Hor spRule) top bottom) = \(width, height) ->
      let availHeight = fromIntegral (height - 1)
          (topHeight, bottomHeight) = splitByRule spRule availHeight
          border = V.charFill (V.defAttr `V.withForeColor` V.green) '-' width 1
       in top (width, topHeight) V.<-> border V.<-> bottom (width, bottomHeight)

    alg (LeafF bufInfo) = \(width, height) -> renderView (width, height) bufInfo

renderView :: (Width, Height) -> View Buffer -> V.Image
renderView (width, height) bufView = appendActiveBar $ V.resize width availHeight $ applyAttrs atts txt
  where
    appendActiveBar img =
      if isActive
        then img V.<-> V.charFill (V.defAttr `V.withForeColor` V.magenta) '-' width 1
        else img
    availHeight = if isActive then height - 1
                              else height
    txt = bufView ^. viewPayload . rope
    atts = bufView ^. viewPayload . styles & fmap (fmap convertStyle)
    isActive = bufView ^. active
