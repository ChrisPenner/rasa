{-# language
  GeneralizedNewtypeDeriving
#-}
module Rasa.Internal.Styles
  ( fg
  , bg
  , flair
  , Color(..)
  , Flair(..)
  , Style(..)
  , Styles
  , addStyleProvider
  , getStyles
  ) where

import Rasa.Internal.Range
import Rasa.Internal.BufAction
import Rasa.Internal.Listeners

import Control.Applicative

import Data.Default

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

type Styles = [Span CrdRange Style]
newtype StyleMap =
  StyleMap Styles
  deriving (Show, Eq, Monoid)

instance Default StyleMap where
  def = StyleMap []

-- | Create a new 'Style' with the given 'Color' as the foreground.
fg :: Color -> Style
fg a = Style (Just a, Nothing, Nothing)

-- | Create a new 'Style' with the given 'Color' as the background.
bg :: Color -> Style
bg a = Style (Nothing, Just a, Nothing)

-- Create a new 'Style' with the given 'Flair' as its flair.
flair :: Flair -> Style
flair a = Style (Nothing, Nothing, Just a)

data ComputeStyles = ComputeStyles

addStyleProvider :: BufAction Styles -> BufAction ListenerId
addStyleProvider provider = addBufListener (const provider :: ComputeStyles -> BufAction Styles)

getStyles :: BufAction Styles
getStyles = dispatchBufEvent ComputeStyles
