{-# language
    TemplateHaskell
  , ExistentialQuantification
  , RankNTypes
  , OverloadedStrings
#-}
module Rasa.Ext.Views.Internal.Widgets
  ( Widgets
  , HasWidgets(..)
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
import Rasa.Ext.Views.Internal.AnyRenderable
import Rasa.Ext.Views.Internal.ActiveBar
import Rasa.Ext.Views.Internal.StatusBar

import Control.Lens
import Data.Maybe
import Data.Monoid


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

class HasWidgets r where
  computeWidgets :: r -> App Widgets

-- | This represents types which can provide a set of widgets
instance HasWidgets View where
  computeWidgets vw = do
    rest <- case vw^.viewable of
              EmptyView -> return mempty
              (BufView br) -> getBufWidgets br
    return $ activeBar `mappend` rest
    where
      activeBar =
        if vw^.active
          then mempty & bottomBar .~ [AnyRenderable ActiveBar]
          else mempty

      getBufWidgets br = fmap (fromMaybe mempty) . bufDo br $ do
        mainWidgets <- getWidgets
        topStatusBar <- getTopStatusBar
        bottomStatusBar <- getBottomStatusBar
        return $ mainWidgets <> widgetOf bottomBar bottomStatusBar <> widgetOf topBar topStatusBar

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
