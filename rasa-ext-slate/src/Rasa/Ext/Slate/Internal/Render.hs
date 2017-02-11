{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Ext.Slate.Internal.Render (render) where

import Rasa.Ext
import Rasa.Ext.Views
import Rasa.Ext.Style
import Rasa.Ext.Slate.Internal.State
import Rasa.Ext.Slate.Internal.Attributes
import Data.Functor.Foldable
import Data.Bifunctor

import qualified Graphics.Vty as V
import Control.Lens
import Control.Monad.IO.Class

import qualified Yi.Rope as Y

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

-- | Divides up available space according to the given 'SplitRule'.
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

-- | Recursively render components of a Window to a 'V.Image' combining the results in the proper locations.
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

-- | Render a given 'View' to a 'V.Image' given the context of the associated buffer and a size to render it in.
renderView :: (Width, Height) -> (View, Buffer) -> V.Image
renderView (width, height) (vw, buf) = appendActiveBar . resize . addEndBar $ textImage
  where
    trimText :: Y.YiString -> Y.YiString
    trimText = Y.concat . take height . drop (vw^.scrollPos) . Y.lines'
    resize :: V.Image -> V.Image
    resize = V.resize availWidth height
    textImage :: V.Image
    textImage = applyAttrs adjustedStyles txt
    txt :: Y.YiString
    txt = buf^.text & trimText
    adjustedStyles :: [Span CrdRange V.Attr]
    adjustedStyles = bimap adjustStylePositions convertStyle <$> buf^.bufExt.styles
    adjustStylePositions :: CrdRange -> CrdRange
    adjustStylePositions = both.coordRow -~ vw^.scrollPos
    sepBar :: V.Image
    sepBar = V.charFill (V.defAttr `V.withStyle` V.underline) ' ' width 1
    addEndBar :: V.Image -> V.Image
    addEndBar = (V.<-> sepBar)
    appendActiveBar :: V.Image -> V.Image
    appendActiveBar i
      | vw^.active = i V.<|> V.charFill (V.defAttr `V.withForeColor` V.magenta) '|' 1 height
      | otherwise = i
    availWidth :: Height
    availWidth = if vw^.active then width - 1
                                else width

