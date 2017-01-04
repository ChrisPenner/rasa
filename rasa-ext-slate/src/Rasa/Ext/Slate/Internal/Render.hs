{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Ext.Slate.Internal.Render (render) where

import Rasa.Ext
import Rasa.Ext.Viewports
import Rasa.Ext.Style
-- import Rasa.Ext.StatusBar (left, center, right)
import Rasa.Ext.Slate.Internal.State
import Rasa.Ext.Slate.Internal.Attributes
import Control.Monad.IO.Class

import qualified Graphics.Vty as V
import qualified Yi.Rope as Y
import Control.Lens

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
  Viewports vp <- getViewports
  bufs <- collectBuffers
  let img = renderWindow (width, height) $ fmap (bufs !!) vp
      pic = V.picForImage img

  -- bufImg <- focusDo $ renderBuf (width, height - 1)
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

-- | Given a window size, creates a 'BufAction' which will return an image representing the buffer it's run in.
collectBuffers :: Action [(Y.YiString, [Span V.Attr])]
collectBuffers = bufDo $ do
  txt <- use rope
  atts <- fmap (fmap convertStyle) <$> use styles
  return [(txt, atts)]

renderWindow :: (Width, Height) -> Window (Y.YiString, [Span V.Attr]) -> V.Image
renderWindow (width, height) (VSplit (SplitInfo divider) left right) =
     renderWindow (ceiling $ fromIntegral width * divider, height) left
  V.<|> renderWindow (floor $ fromIntegral width * (1 - divider), height) right

renderWindow (width, height) (HSplit (SplitInfo divider) top bottom) =
  renderWindow (width, ceiling $ fromIntegral height * divider) top
  V.<-> renderWindow (width, floor $ fromIntegral height * (1 - divider)) bottom

renderWindow (width, height) (Single viewport) =
  renderBuf (width, height) viewport

renderBuf :: (Width, Height) -> (Y.YiString, [Span V.Attr]) -> V.Image
renderBuf (width, height) (txt, atts)= V.resize width height $ applyAttrs atts txt

