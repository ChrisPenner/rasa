{-# language
    TemplateHaskell
  , ExistentialQuantification
  , RankNTypes
#-}
module Rasa.Ext.Views.Internal.Widgets
  ( Widgets
  , RenderWidgets(..)
  , addTopBar
  , addBottomBar
  , addLeftBar
  , addRightBar
  , topBar
  , bottomBar
  , leftBar
  , rightBar
  ) where

import Rasa.Ext
import Rasa.Ext.Views.Internal.Views

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Monoid

data ARenderable =
  forall r. Renderable r => ARenderable r

instance Renderable ARenderable where
  render width height scrollPos (ARenderable r) = render width height scrollPos r


data Widgets = Widgets
  { _topBar :: [ARenderable]
  , _bottomBar :: [ARenderable]
  , _leftBar :: [ARenderable]
  , _rightBar :: [ARenderable]
  }

makeLenses ''Widgets

instance Monoid Widgets where
  mempty = Widgets mempty mempty mempty mempty
  (Widgets a b c d) `mappend` (Widgets a' b' c' d') =
    Widgets (a<>a') (b<>b') (c<>c') (d<>d')

instance Default Widgets where
  def = mempty

class RenderWidgets r where
  renderWidgets :: r -> Action Widgets

instance RenderWidgets Viewable where
  renderWidgets EmptyView = return def
  renderWidgets (BufView br) = fromMaybe mempty <$> bufDo br getWidgets

instance RenderWidgets View where
  renderWidgets vw = renderWidgets (vw^.viewable)

data GetWidgets = GetWidgets

widgetOf :: Renderable r => Lens' Widgets [ARenderable] -> r -> Widgets
widgetOf l r = def & l .~ [ARenderable r]

mkListenerFor :: Renderable r => Lens' Widgets [ARenderable] -> BufAction r -> BufAction ListenerId
mkListenerFor l bufAction = addBufListener (const (widgetOf l <$> bufAction) :: GetWidgets -> BufAction Widgets)

addLeftBar :: Renderable r => BufAction r -> BufAction ListenerId
addLeftBar = mkListenerFor leftBar

addRightBar :: Renderable r => BufAction r -> BufAction ListenerId
addRightBar = mkListenerFor rightBar

addTopBar :: Renderable r => BufAction r -> BufAction ListenerId
addTopBar = mkListenerFor topBar

addBottomBar :: Renderable r => BufAction r -> BufAction ListenerId
addBottomBar = mkListenerFor bottomBar

getWidgets :: BufAction Widgets
getWidgets = dispatchBufEvent GetWidgets
