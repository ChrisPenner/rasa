{-# language
    FlexibleInstances
#-}
module Rasa.Internal.Utility
  ( Width
  , Height
  , Renderable(..)
  , RenderInfo(..)
  , ScrollPos
  , cropToViewport
  , styleText
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

data RenderInfo =
  RenderInfo Y.YiString Styles

-- | Appends to RenderInfo by appending the text and styles while preserving
-- proper text/style alignment
instance Monoid RenderInfo where
  mempty = RenderInfo mempty mempty
  RenderInfo txtA stylesA `mappend` RenderInfo txtB stylesB =
    RenderInfo
      (txtA `mappend` txtB)
      (mappend stylesA $ first (moveRange (sizeOf txtA)) <$> stylesB)

-- | Represents how to render an entity
class Renderable r where
  render :: Width -> Height -> ScrollPos -> r -> Action (Maybe RenderInfo)

instance Renderable r => Renderable (Maybe r) where
  render width height scrollPos (Just r) = render width height scrollPos r
  render _ _ _ Nothing = return Nothing

instance Renderable BufRef where
  render _ height scrollPos bufRef = bufDo bufRef $ do
    txt <- getText
    styles <- getStyles
    return $ cropToViewport height scrollPos (RenderInfo txt styles)

instance Renderable Y.YiString where
  render _ height scrollPos txt = return . Just $ cropToViewport height scrollPos (RenderInfo txt [])

instance Renderable RenderInfo where
  render _ _ _ r = return (Just r)

type ScrollPos = Int
-- | Crop to only the in-view portion.
cropToViewport :: Height -> ScrollPos -> RenderInfo -> RenderInfo
cropToViewport height scrollAmt (RenderInfo txt styles) = RenderInfo trimmedText adjustedStyles
  where
    adjustedStyles :: Styles
    adjustedStyles = first adjustStylePositions <$> styles
    adjustStylePositions :: CrdRange -> CrdRange
    adjustStylePositions = both.coordRow -~ scrollAmt
    trimmedText :: Y.YiString
    trimmedText = Y.concat . take height . drop scrollAmt . Y.lines' $ txt

-- | Add a style to some text resulting in a 'RenderInfo'
styleText :: Y.YiString -> Style -> RenderInfo
styleText txt style = RenderInfo txt [Span (Range (Coord 0 0) (sizeOf txt)) style]
