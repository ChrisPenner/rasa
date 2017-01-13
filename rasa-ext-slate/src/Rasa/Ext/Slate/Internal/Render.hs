{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Ext.Slate.Internal.Render (render) where

import Rasa.Ext
import Rasa.Ext.Views
import Rasa.Ext.Style
-- import Rasa.Ext.StatusBar (left, center, right)
import Rasa.Ext.Slate.Internal.State
import Rasa.Ext.Slate.Internal.Attributes
import Data.Functor.Foldable

import qualified Graphics.Vty as V
import Control.Lens
import Control.Monad.IO.Class

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
  mBufViews <- getBufferViews
  liftIO $ appendFile "bufs.log" (show mBufViews)
  case mBufViews of
    Nothing -> return ()
    Just win -> 
      let img = renderWindow (width, height) win
          pic = V.picForImage img
       in getVty >>= liftIO . flip V.update pic

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

renderWindow :: (Width, Height) -> BiTree Split (View, Buffer) -> V.Image
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

renderView :: (Width, Height) -> (View, Buffer) -> V.Image
renderView (width, height) (vw, buf) = appendActiveBar $ V.resize width availHeight $ applyAttrs atts txt
  where
    appendActiveBar img =
      if isActive
        then img V.<-> V.charFill (V.defAttr `V.withForeColor` V.magenta) '-' width 1
        else img
    availHeight = if isActive then height - 1
                              else height
    txt = buf^.text
    atts = buf^.styles & fmap (fmap convertStyle)
    isActive = vw ^. active
