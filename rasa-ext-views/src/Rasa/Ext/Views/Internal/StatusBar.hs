{-# language
    OverloadedStrings
#-}
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

-- | Returns a Renderable StatusBar for a given buffer
getTopStatusBar :: BufAction StatusBar
getTopStatusBar = StatusBar <$> dispatchBufEvent GetTopStatusBar

-- | Returns a Renderable StatusBar for a given buffer
getBottomStatusBar :: BufAction StatusBar
getBottomStatusBar = StatusBar <$> dispatchBufEvent GetBottomStatusBar

-- | This registers a 'BufAction' which results in a renderable and runs it
-- at render time to add the resulting 'Renderable' to the status bar.
addTopStatus :: Renderable r => BufAction r -> BufAction ListenerId
addTopStatus bufAction = addBufListener (const (toRenderList <$> bufAction) :: GetTopStatusBar -> BufAction [AnyRenderable])
  where toRenderList x = [AnyRenderable x]

addBottomStatus :: Renderable r => BufAction r -> BufAction ListenerId
addBottomStatus bufAction = addBufListener (const (toRenderList <$> bufAction) :: GetBottomStatusBar -> BufAction [AnyRenderable])
  where toRenderList x = [AnyRenderable x]

instance Renderable StatusBar where
  render width _ scrollAmt (StatusBar chunks) = do
    parts <- traverse (render eachWidth 1 scrollAmt) chunks
    return . Just . mconcat . intersperse (styleText " | " (fg Red)) . catMaybes $ parts
    where
      num = length chunks
      eachWidth = width `div` num
