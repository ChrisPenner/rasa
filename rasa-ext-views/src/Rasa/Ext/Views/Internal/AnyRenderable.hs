{-# language
  ExistentialQuantification
#-}
module Rasa.Ext.Views.Internal.AnyRenderable 
  ( AnyRenderable(..)
  ) where

import Rasa.Ext

data AnyRenderable =
  forall r. Renderable r => AnyRenderable r

instance Renderable AnyRenderable where
  render width height scrollAmt (AnyRenderable r) = render width height scrollAmt r
