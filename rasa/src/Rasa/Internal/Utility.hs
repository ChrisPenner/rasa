{-# language
    FlexibleInstances
#-}
module Rasa.Internal.Utility
  ( Width
  , Height
  , Renderable(..)
  , RenderInfo
  , cropToViewport
  ) where

import Rasa.Internal.ActionMonads
import Rasa.Internal.Editor
import Rasa.Internal.Actions
import Rasa.Internal.Styles
import Rasa.Internal.BufAction
import Rasa.Internal.Range

import Control.Lens
import Data.Bifunctor

import qualified Yi.Rope as Y

type Width = Int
type Height = Int
type RenderInfo = (Y.YiString, Styles)

-- | Represents how to render an entity
class Renderable r where
  render :: Width -> Height -> ScrollPos -> r -> Action (Maybe RenderInfo)

instance Renderable r => Renderable (Maybe r) where
  render width height scrollPos (Just r) = render width height scrollPos r
  render width height scrollPos Nothing = return Nothing

instance Renderable BufRef where
  render _ height scrollPos bufRef = bufDo bufRef $ do
    txt <- getText
    styles <- getStyles
    return $ cropToViewport height scrollPos (txt, styles)

instance Renderable Y.YiString where
  render _ height scrollPos txt = return . Just $ cropToViewport height scrollPos (txt, [])

instance Renderable RenderInfo where
  render _ _ _ r = return (Just r)

type ScrollPos = Int
-- | Crop to only the in-view portion.
cropToViewport :: Height -> ScrollPos -> RenderInfo -> RenderInfo
cropToViewport height scrollAmt (txt, styles) = (trimmedText, adjustedStyles)
  where
    adjustedStyles :: Styles
    adjustedStyles = first adjustStylePositions <$> styles
    adjustStylePositions :: CrdRange -> CrdRange
    adjustStylePositions = both.coordRow -~ scrollAmt
    trimmedText :: Y.YiString
    trimmedText = Y.concat . take height . drop scrollAmt . Y.lines' $ txt
