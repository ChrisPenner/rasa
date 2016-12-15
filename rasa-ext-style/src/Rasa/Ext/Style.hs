{-# language TemplateHaskell #-}
module Rasa.Ext.Style (styles, addStyle, fg, bg, flair, Color(..), Flair(..), IStyle(..), Style(..)) where

import Rasa.Ext
import Control.Lens

import Data.Default
import Control.Applicative

import Data.List (insert)

data Color =
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | DefColor
  deriving (Show, Eq)

data Flair =
    Standout
  | Underline
  | ReverseVideo
  | Blink
  | Dim
  | Bold
  | DefFlair
  deriving (Show, Eq)

newtype Style = Style (Maybe Color, Maybe Color, Maybe Flair)
  deriving (Show, Eq)

instance Default Style where
  def = Style (Just DefColor, Just DefColor, Just DefFlair)

instance Monoid Style where
  Style (a, b, c) `mappend` Style (a', b', c') = Style (a' <|> a, b' <|> b, c' <|> c)

  mempty = Style (Nothing, Nothing, Nothing)

-- Style with an index into the text or buffer
data IStyle =
  IStyle Int Style
  deriving (Show, Eq)

newtype Styles =
  Styles {
  -- This list must always stay sorted by the index of the styles
    _styles' :: [IStyle]
         } deriving (Show, Eq)

makeLenses ''Styles

styles :: Lens' Buffer [IStyle]
styles = bufExt.styles'

instance Default Styles where
  def = Styles []

instance Ord IStyle where
  compare (IStyle i _) (IStyle i' _) = compare i i'


fg, bg :: Color -> Style
fg a = Style (Just a, Nothing, Nothing)
bg a = Style (Nothing, Just a, Nothing)

flair :: Flair -> Style
flair a = Style (Nothing, Nothing, Just a)

-- Inserts a style into a buffer's style list in sorted order
addStyle :: IStyle -> Buffer -> Buffer
addStyle style = styles %~ insert style
