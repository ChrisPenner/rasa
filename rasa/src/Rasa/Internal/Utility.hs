module Rasa.Internal.Utility 
  ( Width
  , Height
  , Renderable(..)
  , RenderInfo
  ) where

import Rasa.Internal.ActionMonads
import Rasa.Internal.Editor
import Rasa.Internal.Range
import Rasa.Internal.Actions
import Rasa.Internal.Styles
import Rasa.Internal.BufAction

import qualified Yi.Rope as Y

type Width = Int
type Height = Int
type RenderInfo = (Y.YiString, [Span CrdRange Style])
class Renderable r where
  render :: r -> Width -> Height -> Action (Maybe RenderInfo)

instance Renderable BufRef where
  render bufRef _ _ = bufDo bufRef $ (,) <$> getText <*> getStyles
