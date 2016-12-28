{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Renderer.Slate.Render (render) where

import Rasa.Ext
import Rasa.Ext.Style
import Rasa.Ext.StatusBar (left, center, right)
import Rasa.Renderer.Slate.State
import Rasa.Renderer.Slate.Attributes
import Control.Monad.IO.Class

import qualified Graphics.Vty as V
import Control.Lens

import qualified Data.Text as T
import Data.Monoid

-- | Given a window size, creates a 'BufAction' which will return an image representing the buffer it's run in.
renderBuf :: (Int, Int) -> BufAction V.Image
renderBuf (width, height) = do
  txt <- use rope
  atts <- fmap (fmap convertStyle) <$> use styles
  let img = applyAttrs atts txt
  return $ V.resize width height img

-- | Get the current terminal size.
getSize :: Action (Int, Int)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v

-- | Render the Editor
render :: Action ()
render = do
  (width, height) <- getSize
  bufImg <- focusDo $ renderBuf (width, height - 1)
  statusBar <- renderStatus width
  let img = bufImg V.<-> statusBar
      pic = V.picForImage img
  v <- getVty
  liftIO $ V.update v pic

-- | Render the status bar.
renderStatus :: Int -> Action V.Image
renderStatus width = focusDo $ do
  statuses <- use bufExt
  let spacer = T.replicate spacerSize " "
      spacerSize = (width - T.length (T.concat joinedParts)) `div` 2
      barParts = [ statuses^.left, statuses^.center, statuses^.right ]
      addSpacer = (<> spacer)
      joinedParts = T.intercalate " | " <$> barParts
      fullLine = foldMap addSpacer joinedParts
  return $ V.text' V.defAttr fullLine
