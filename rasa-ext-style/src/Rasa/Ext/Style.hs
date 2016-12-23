{-# language TemplateHaskell #-}
module Rasa.Ext.Style (styleMain, styles, addStyle, fg, bg, flair, Color(..), Flair(..),  Style(..)) where

import Rasa.Ext
import Rasa.Ext.Directive
import Rasa.Ext.Scheduler
import Control.Lens

import Data.Default
import Control.Applicative

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

newtype Styles =
  Styles {
  -- This list must always stay sorted by the index of the styles
  _styles' :: [Span Style]
         } deriving (Show, Eq)

makeLenses ''Styles

styles :: Lens' Buffer [Span Style]
styles = bufExt.styles'

instance Default Styles where
  def = Styles []

fg, bg :: Color -> Style
fg a = Style (Just a, Nothing, Nothing)
bg a = Style (Nothing, Just a, Nothing)

flair :: Flair -> Style
flair a = Style (Nothing, Nothing, Just a)

-- Inserts a style into a buffer's style list in sorted order
addStyle :: Range -> Style -> BufAction ()
addStyle r style = do
  txt <- use rope
  let (Offset s, Offset e) = asOffsets txt r
      sp = Span s e style
  styles %= (sp:)

styleMain :: Scheduler ()
styleMain = afterRender $ bufDo $ styles .= []
