{-# language TemplateHaskell #-}
module Rasa.Ext.Style (style, styles, addStyle, fg, bg, flair, Color(..), Flair(..),  Style(..)) where

import Rasa.Ext
import Control.Lens

import Data.Default
import Control.Applicative

-- | These represent the possible colors for 'fg' or 'bg'.
-- 'DefColor' represents the terminal's default color.
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

-- | These represent the possible extra attributes which may be applied.
-- 'DefFlair' represents the terminal's default text attributes.
data Flair =
    Standout
  | Underline
  | ReverseVideo
  | Blink
  | Dim
  | Bold
  | DefFlair
  deriving (Show, Eq)

-- | A container which holds a foreground color, background color, and a flair.
-- a 'Nothing' represents that we should not change that attribute.
newtype Style = Style (Maybe Color, Maybe Color, Maybe Flair)
  deriving (Show, Eq)

instance Default Style where
  def = Style (Just DefColor, Just DefColor, Just DefFlair)

-- | The monoid instance replaces any attributes which have a 'Just' in the new 'Style'
-- and persists any that are 'Nothing' in the new style (using 'Data.Alternative' for 'Data.Maybe')
instance Monoid Style where
  Style (a, b, c) `mappend` Style (a', b', c') = Style (a' <|> a, b' <|> b, c' <|> c)

  mempty = Style (Nothing, Nothing, Nothing)

newtype Styles =
  Styles {
  -- This list must always stay sorted by the index of the styles
  _styles' :: [Span Style]
         } deriving (Show, Eq)

makeLenses ''Styles

-- | A lens over the styles stored in the current buffer.
styles :: HasBuffer s => Lens' s [Span Style]
styles = bufExt.styles'

instance Default Styles where
  def = Styles []

-- | Create a new 'Style' with the given 'Color' as the foreground.
fg :: Color -> Style
fg a = Style (Just a, Nothing, Nothing)

-- | Create a new 'Style' with the given 'Color' as the background.
bg :: Color -> Style
bg a = Style (Nothing, Just a, Nothing)

-- Create a new 'Style' with the given 'Flair' as its flair.
flair :: Flair -> Style
flair a = Style (Nothing, Nothing, Just a)

-- | Applies a style over a given range in the buffer's style list.
addStyle :: Range -> Style -> BufAction ()
addStyle r st = styles %= (Span r st:)

-- | The main export for the style extension. Add this to your user config.
--
-- e.g.
--
-- > rasa $ do
-- >    style
-- >    ...
style :: Action ()
style = afterEveryRender_ $ buffersDo_ $ styles .= []
