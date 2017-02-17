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
  mInfo <- render width height scrollAmt (vw^.viewable)
  widgets <- renderWidgets vw
  top <- V.vertCat . catMaybes <$> traverse (renderHorBar width) (widgets^.topBar)
  bottom <- V.vertCat . catMaybes <$> traverse (renderHorBar width) (widgets^.bottomBar)
  left <- V.vertCat . catMaybes <$> traverse (renderVertBar height) (widgets^.leftBar)
  right <- V.vertCat . catMaybes <$> traverse (renderVertBar height) (widgets^.rightBar)
  let remainingHeight = height - (V.imageHeight top + V.imageHeight bottom)
      remainingWidth = width - (V.imageWidth left + V.imageWidth right)
      img = maybe V.emptyImage (V.resize remainingWidth remainingHeight . applyAttrs) mInfo
  return $ top V.<-> (left V.<|> img V.<|> right) V.<-> bottom
  where
    scrollAmt = vw^.scrollPos
    renderHorBar :: forall r. Renderable r => Width -> r -> Action (Maybe V.Image)
    renderHorBar w r = fmap applyAttrs <$> render w 1 scrollAmt r
    renderVertBar :: forall r. Renderable r => Height -> r -> Action (Maybe V.Image)
    renderVertBar h r = fmap applyAttrs <$> render 1 h scrollAmt r

type ScrollPos = Int

-- | Renders text and styles to an image
renderToImage :: Width -> Height -> RenderInfo -> V.Image
renderToImage width height = V.resize width height . applyAttrs
