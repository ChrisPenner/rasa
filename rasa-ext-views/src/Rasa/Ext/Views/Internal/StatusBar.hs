{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language ExistentialQuantification #-}
module Rasa.Ext.Views.Internal.StatusBar
  ( getTopStatusBar
  , getBottomStatusBar
  , addTopStatus
  , addBottomStatus
  ) where

import Rasa.Ext
import Rasa.Ext.Views.Internal.AnyRenderable

import Data.List
import Data.Maybe

data GetTopStatusBar = GetTopStatusBar
data GetBottomStatusBar = GetBottomStatusBar
data StatusBar = StatusBar [AnyRenderable]

-- | Returns a Renderable StatusBar
getTopStatusBar :: HasEvents s => Action s StatusBar
getTopStatusBar = StatusBar <$> dispatchLocalEvent GetTopStatusBar

-- | Returns a Renderable StatusBar
getBottomStatusBar :: HasEvents s => Action s StatusBar
getBottomStatusBar = StatusBar <$> dispatchLocalEvent GetBottomStatusBar

-- | This registers an 'Action' which results in a renderable and runs it
-- at render time to add the resulting 'Renderable' to the status bar.
addTopStatus :: forall s r. (Renderable r, HasEvents s) => Action s r -> Action s ListenerId
addTopStatus action = addLocalListener (const (toRenderList <$> action) :: GetTopStatusBar -> Action s [AnyRenderable])
  where toRenderList x = [AnyRenderable x]

-- | bottom version of 'addTopStatus'
addBottomStatus :: forall r s. (Renderable r, HasEvents s) => Action s r -> Action s ListenerId
addBottomStatus action = addLocalListener (const (toRenderList <$> action) :: GetBottomStatusBar -> Action s [AnyRenderable])
  where toRenderList x = [AnyRenderable x]

instance Renderable StatusBar where
  render width _ scrollAmt (StatusBar chunks) = do
    parts <- traverse (render eachWidth 1 scrollAmt) chunks
    return . Just . mconcat . intersperse (styleText " | " (fg Red)) . catMaybes $ parts
    where
      num = length chunks
      eachWidth = width `div` num
