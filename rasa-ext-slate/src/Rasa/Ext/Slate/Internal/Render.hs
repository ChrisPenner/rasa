{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Ext.Slate.Internal.Render (render) where

import Rasa.Ext
import Rasa.Ext.Views
import Rasa.Ext.Style
-- import Rasa.Ext.StatusBar (left, center, right)
import Rasa.Ext.Slate.Internal.State
import Rasa.Ext.Slate.Internal.Attributes
import Control.Monad.IO.Class

import qualified Graphics.Vty as V
import Control.Lens
import Control.Monad.State

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
  wins <- use windows
  bufs <- bufDo $ (:[]) <$> get
  let addContext v = (bufs !! view bufIndex v, v)
      img = renderWindow (width, height) $ fmap addContext wins
      pic = V.picForImage img

  -- statusBar <- renderStatus width
  -- let img = bufImg V.<-> statusBar
  v <- getVty
  liftIO $ V.update v pic

-- -- | Render the status bar.
-- renderStatus :: Int -> Action V.Image
-- renderStatus width = focusDo $ do
--   statuses <- use bufExt
--   let spacer = T.replicate spacerSize " "
--       spacerSize = (width - T.length (T.concat joinedParts)) `div` 2
--       barParts = [ statuses^.left, statuses^.center, statuses^.right ]
--       addSpacer = (<> spacer)
--       joinedParts = T.intercalate " | " <$> barParts
--       fullLine = foldMap addSpacer joinedParts
--   return $ V.text' V.defAttr fullLine

splitByRule :: SplitRule -> Int -> (Int, Int)
splitByRule (Ratio r) sz = (start, end)
  where
    start = ceiling $ fromIntegral sz * r
    end = floor $ fromIntegral sz * (1 - r)

splitByRule (FromStart amt) sz = (start, end)
  where
    start = min sz amt
    end = sz - start

splitByRule (FromEnd amt) sz = (start, end)
  where
    start = sz - end
    end = min sz amt


renderWindow :: (Width, Height) -> Window (Buffer, View) -> V.Image
renderWindow (width, height) (Split Vert (SplitInfo spRule) left right) =
        renderWindow (leftWidth, height) left
  V.<|> border
  V.<|> renderWindow (rightWidth, height) right
    where
      availWidth = fromIntegral (width - 1)
      (leftWidth, rightWidth) = splitByRule spRule availWidth
      border = V.charFill (V.defAttr `V.withForeColor` V.green) '|' 1 height

renderWindow (width, height) (Split Hor (SplitInfo spRule) top bottom) =
        renderWindow (width, topHeight) top
  V.<-> border
  V.<-> renderWindow (width, bottomHeight) bottom
    where
      availHeight = fromIntegral (height - 1)
      (topHeight, bottomHeight) = splitByRule spRule availHeight
      border = V.charFill (V.defAttr `V.withForeColor` V.green) '-' width 1

renderWindow (width, height) (Single (buffer, viewInfo)) =
  renderView (width, height) viewInfo buffer

renderView :: (Width, Height) -> View -> Buffer -> V.Image
renderView (width, height) (View _ (ScrollPos scroll) _) buffer = 
  V.resize width height $ V.translateY (-transAmt) img
    where
      txt = buffer^.rope
      atts = fmap convertStyle <$> buffer^.styles
      img = applyAttrs atts txt
      imHeight = V.imageHeight img
      transAmt = min imHeight scroll
