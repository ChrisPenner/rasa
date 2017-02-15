{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Ext.Slate.Internal.Render
  ( renderAll
  , Renderable(..)
  ) where

import Rasa.Ext
import Rasa.Ext.Views
import Rasa.Ext.Slate.Internal.State
import Rasa.Ext.Slate.Internal.Attributes
import Data.Functor.Foldable
import Data.Bifunctor

import qualified Graphics.Vty as V
import Control.Lens
import Control.Monad.IO.Class

import qualified Yi.Rope as Y

-- | Get the current terminal size.
getSize :: Action (Width, Height)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v

-- | Render the Editor
renderAll :: Action ()
renderAll = do
  (width, height) <- getSize
  Views mViews <- getViews
  case mViews of
    Nothing -> return ()
    Just win -> do
      img <- renderWindow win width height
      let pic = V.picForImage img
      v <- getVty
      liftIO $ V.update v pic

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
renderWindow :: BiTree Split View -> Width -> Height -> Action V.Image
renderWindow win = cata alg win
  where
    alg (BranchF (Split Vert spRule) left right) = \width height ->
      let availWidth = fromIntegral (width - 1)
          (leftWidth, rightWidth) = splitByRule spRule availWidth
          border = V.charFill (V.defAttr `V.withForeColor` V.green) '|' 1 height
       in do
         leftView <- left leftWidth height 
         rightView <-  right rightWidth height
         return $ leftView V.<|> border V.<|> rightView

    alg (BranchF (Split Hor spRule) top bottom) = \width height ->
      let availHeight = fromIntegral (height - 1)
          (topHeight, bottomHeight) = splitByRule spRule availHeight
          border = V.charFill (V.defAttr `V.withForeColor` V.green) '-' width 1
       in do
         topView <- top width topHeight
         bottomView <- bottom width bottomHeight
         return $ topView V.<-> border V.<-> bottomView

    alg (LeafF vw) = \width height -> renderView width height vw

-- | Render a given 'View' to a 'V.Image' given the context of the associated buffer and a size to render it in.
renderView :: Width -> Height -> View -> Action V.Image
renderView width height vw = do
  mInfo <- render vw width height
  return $ case mInfo of
             Nothing -> V.emptyImage
             Just (txt, styles) -> appendActiveBar . resize . addEndBar $ renderText height (vw^.scrollPos) txt styles
  where
    appendActiveBar :: V.Image -> V.Image
    appendActiveBar i
      | vw^.active = i V.<|> V.charFill (V.defAttr `V.withForeColor` V.magenta) '|' 1 height
      | otherwise = i
    sepBar :: V.Image
    sepBar = V.charFill (V.defAttr `V.withStyle` V.underline) ' ' width 1
    addEndBar :: V.Image -> V.Image
    addEndBar = (V.<-> sepBar)
    resize :: V.Image -> V.Image
    resize = V.resize availWidth height
    availWidth :: Height
    availWidth = if vw^.active then width - 1
                                else width

type ScrollPos = Int
renderText :: Height -> ScrollPos -> Y.YiString -> StyleMap -> V.Image
renderText height scrollAmt txt styles = textImage
  where
    trimText :: Y.YiString -> Y.YiString
    trimText = Y.concat . take height . drop scrollAmt . Y.lines'
    textImage :: V.Image
    textImage = applyAttrs adjustedStyles (trimText txt)
    adjustedStyles :: [Span CrdRange V.Attr]
    adjustedStyles = bimap adjustStylePositions convertStyle <$> styles
    adjustStylePositions :: CrdRange -> CrdRange
    adjustStylePositions = both.coordRow -~ scrollAmt

