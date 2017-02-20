{-# language
    OverloadedStrings
#-}
module Rasa.Ext.Views.Internal.StatusBar
  ( getStatusBar
  , addStatus
  ) where

import Rasa.Ext
import Rasa.Ext.Views.Internal.AnyRenderable

import Data.List

data GetStatusBar = GetStatusBar
data StatusBar = StatusBar [AnyRenderable]

-- | Returns a Renderable StatusBar for a given buffer
getStatusBar :: BufAction StatusBar
getStatusBar = StatusBar <$> dispatchBufEvent GetStatusBar

-- | This registers a 'BufAction' which results in a renderable and runs it
-- at render time to add the resulting 'Renderable' to the status bar.
addStatus :: Renderable r => BufAction r -> BufAction ListenerId
addStatus bufAction = addBufListener (const (toRenderList <$> bufAction) :: GetStatusBar -> BufAction [AnyRenderable])
  where toRenderList x = [AnyRenderable x]

instance Renderable StatusBar where
  render width _ scrollAmt (StatusBar chunks) = do
    parts <- traverse (render eachWidth 1 scrollAmt) chunks
    return . mconcat $ intersperse (Just $ styleText " | " (fg Red)) parts
    where
      num = length chunks
      eachWidth = width `div` num
