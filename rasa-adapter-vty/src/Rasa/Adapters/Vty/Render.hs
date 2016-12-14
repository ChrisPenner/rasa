{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Adapters.Vty.Render (render') where

import Rasa.Ext
import Rasa.Editor
import Rasa.View
import Rasa.Adapters.Vty.State
import Rasa.Adapters.Vty.Attributes
import Control.Monad.IO.Class

import qualified Graphics.Vty as V
import Control.Lens

class Renderable a b where
    render :: Size -> a -> b

instance Renderable Editor V.Image where
    render sz = view $ focusedBuf . to (render sz)

instance Renderable Buffer V.Image where
    render (width, _) = do
        txt <- textWrap width . view text
        atts <- fmap convertIAttr <$> view attrs
        return $ applyAttrs atts txt

getSize :: Alteration (Int, Int)
getSize = do
  v <- getVty
  liftIO $ V.displayBounds $ V.outputIface v

render' :: Alteration ()
render' = do
  eState <- use editor
  sz <- getSize
  let pic = V.picForImage $ render sz eState
  v <- getVty
  liftIO $ V.update v pic
