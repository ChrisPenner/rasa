{-# language
    TemplateHaskell
  , ExistentialQuantification
  , RankNTypes
  , OverloadedStrings
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
import Rasa.Ext.Views.Internal.ActiveBar

import Control.Lens
import Data.Maybe
import Data.Monoid

data AnyRenderable =
  forall r. Renderable r => AnyRenderable r

instance Renderable AnyRenderable where
  render width height scrollAmt (AnyRenderable r) = render width height scrollAmt r


-- | Represents all widgets for a given view. Can be added onto using the Monoid instance.
data Widgets = Widgets
  { _topBar :: [AnyRenderable]
  , _bottomBar :: [AnyRenderable]
  , _leftBar :: [AnyRenderable]
  , _rightBar :: [AnyRenderable]
  }

makeLenses ''Widgets

instance Monoid Widgets where
  mempty = Widgets mempty mempty mempty mempty
  (Widgets a b c d) `mappend` (Widgets a' b' c' d') =
    Widgets (a<>a') (b<>b') (c<>c') (d<>d')

class RenderWidgets r where
  renderWidgets :: r -> Action Widgets

-- | This represents types which can provide a set of widgets
instance RenderWidgets View where
  renderWidgets vw = do
    let activeBar = if vw^.active
        then mempty & bottomBar .~ [AnyRenderable ActiveBar]
        else mempty
    rest <- case vw^.viewable of
      EmptyView -> return mempty
      (BufView br) -> fromMaybe mempty <$> bufDo br getWidgets
    return $ activeBar `mappend` rest

data GetWidgets = GetWidgets

widgetOf :: Renderable r => Lens' Widgets [AnyRenderable] -> r -> Widgets
widgetOf l r = mempty & l .~ [AnyRenderable r]

mkListenerFor :: Renderable r => Lens' Widgets [AnyRenderable] -> BufAction r -> BufAction ListenerId
mkListenerFor l bufAction = addBufListener (const (widgetOf l <$> bufAction) :: GetWidgets -> BufAction Widgets)

-- | Use the computed renderer as a left-bar widget
addLeftBar :: Renderable r => BufAction r -> BufAction ListenerId
addLeftBar = mkListenerFor leftBar

-- | Use the computed renderer as a right-bar widget
addRightBar :: Renderable r => BufAction r -> BufAction ListenerId
addRightBar = mkListenerFor rightBar

-- | Use the computed renderer as a top-bar widget
addTopBar :: Renderable r => BufAction r -> BufAction ListenerId
addTopBar = mkListenerFor topBar

-- | Use the computed renderer as a bottom-bar widget
addBottomBar :: Renderable r => BufAction r -> BufAction ListenerId
addBottomBar = mkListenerFor bottomBar

getWidgets :: BufAction Widgets
getWidgets = dispatchBufEvent GetWidgets
