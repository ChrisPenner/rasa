{-# language
   FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , ExistentialQuantification
#-}
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
import Data.Monoid
import Data.Maybe

import qualified Graphics.Vty as V
import Control.Lens
import Control.Monad.IO.Class

import qualified Yi.Rope as Y

type ScrollPos = Int

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
renderWindow = cata alg
  where
    alg (BranchF (Split Vert spRule) left right) = \width height ->
      let availWidth = max 0 $ fromIntegral (width - 1)
          (leftWidth, rightWidth) = splitByRule spRule availWidth
          border = V.charFill (V.defAttr `V.withForeColor` V.green) '|' 1 height
       in do
         leftView <- left leftWidth height
         rightView <-  right rightWidth height
         return $ leftView V.<|> border V.<|> rightView

    alg (BranchF (Split Hor spRule) top bottom) = \width height ->
      let availHeight = max 0 $ fromIntegral (height - 1)
          (topHeight, bottomHeight) = splitByRule spRule availHeight
          border = V.charFill (V.defAttr `V.withForeColor` V.green) '-' width 1
       in do
         topView <- top width topHeight
         bottomView <- bottom width bottomHeight
         return $ topView V.<-> border V.<-> bottomView

    alg (LeafF vw) = \width height -> renderView width height vw


type Top = V.Image
type Bottom = V.Image
type Left = V.Image
type Right = V.Image

-- | Renders widgets to images
widgetsToImages :: Width -> Height -> ScrollPos -> Widgets -> Action (Top, Bottom, Left, Right)
widgetsToImages width height scrollAmt widgets = do
  top <- V.vertCat . catMaybes <$> traverse (renderHorBar width) (widgets^.topBar)
  bottom <- V.vertCat . catMaybes <$> traverse (renderHorBar width) (widgets^.bottomBar)
  left <- V.horizCat . catMaybes <$> traverse (renderVertBar height) (widgets^.leftBar)
  right <- V.horizCat . catMaybes <$> traverse (renderVertBar height) (widgets^.rightBar)
  return (top, bottom, left, right)
    where
      renderHorBar :: forall r. Renderable r => Width -> r -> Action (Maybe V.Image)
      renderHorBar w r = fmap applyAttrs <$> render w 1 0 r
      renderVertBar :: forall r. Renderable r => Height -> r -> Action (Maybe V.Image)
      renderVertBar h r = fmap applyAttrs <$> render 1 h scrollAmt r

-- | Render a given 'View' to a 'V.Image' given the context of the associated buffer and a size to render it in.
renderView :: Width -> Height -> View -> Action V.Image
renderView width height vw = do
  widgets <- renderWidgets vw
  (top, bottom, left, right)  <- widgetsToImages width height scrollAmt widgets
  let remainingHeight = max 0 $ height - (V.imageHeight top + V.imageHeight bottom)
      remainingWidth = max 0 $ width - (V.imageWidth left + V.imageWidth right)
  bufRenderInfo <- render remainingWidth remainingHeight scrollAmt (vw^.viewable)
  let img = V.resize remainingWidth remainingHeight $ maybe V.emptyImage applyAttrs bufRenderInfo
  return $ top V.<-> (left V.<|> img V.<|> right) V.<-> bottom
  where
    scrollAmt = vw^.scrollPos
