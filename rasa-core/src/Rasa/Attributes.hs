module Rasa.Attributes (fg, bg, style, iattr, Color(..), Style(..), IAttr(..), Attr(..)) where

import Data.Default
import Control.Applicative

-- Attr with an index into the text or buffer
data IAttr =
  IAttr Int Attr
  deriving (Show, Eq)

iattr :: Int -> Attr -> IAttr
iattr = IAttr

instance Ord IAttr where
  compare (IAttr i _) (IAttr i' _) = compare i i'

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

data Style =
    Standout
  | Underline
  | ReverseVideo
  | Blink
  | Dim
  | Bold
  | DefStyle
  deriving (Show, Eq)

newtype Attr = Attr (Maybe Color, Maybe Color, Maybe Style)
  deriving (Show, Eq)

instance Default Attr where
  def = Attr (Just DefColor, Just DefColor, Just DefStyle)

instance Monoid Attr where
  Attr (a, b, c) `mappend` Attr (a', b', c') = Attr (a' <|> a, b' <|> b, c' <|> c)

  mempty = Attr (Nothing, Nothing, Nothing)

fg, bg :: Color -> Attr
fg a = Attr (Just a, Nothing, Nothing)
bg a = Attr (Nothing, Just a, Nothing)

style :: Style -> Attr
style a = Attr (Nothing, Nothing, Just a)
