{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Ext.Slate.Internal.Render (render) where

import Rasa.Ext
import Rasa.Ext.Bufs
import Rasa.Ext.Style
import Rasa.Ext.StatusBar (left, center, right)
import Rasa.Ext.Slate.Internal.State
import Rasa.Ext.Slate.Internal.Attributes


import qualified Graphics.Vty as V
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

-- | Get the current terminal size.
getSize :: Action (Int, Int)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v

-- | Render the Editor
render :: Action ()
render = do
  (width, height) <- getSize
  mBufImg <- focusDo $ renderBuf (width, height - 1)
  case mBufImg of
    Nothing -> return ()
    Just bufImg -> do
      statusBar <- renderStatus width
      let img = bufImg V.<-> statusBar
          pic = V.picForImage img
      v <- getVty
      liftIO $ V.update v pic

-- | Render the status bar.
renderStatus :: Int -> Action V.Image
renderStatus width = fmap fold . focusDo $ do
  statuses <- use bufExt
  let spacer = Y.replicate spacerSize " "
      spacerSize = (width - Y.length (Y.concat joinedParts)) `div` 2
      barParts = [ statuses^.left, statuses^.center, statuses^.right ]
      addSpacer = (<> spacer)
      joinedParts = Y.intercalate " | " <$> barParts
      fullLine = foldMap addSpacer joinedParts
  return $ V.text' V.defAttr (Y.toText fullLine)
