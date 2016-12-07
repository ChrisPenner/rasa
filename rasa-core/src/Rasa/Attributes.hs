module Rasa.Attributes (fg, bg, style, Color(..), Style(..), Attr, IAttr) where

import Data.Default
import Control.Applicative

-- Attr with an index into the text or buffer
newtype IAttr =
  IAttr (Int, Attr)
  deriving (Show, Eq)

instance Ord IAttr where
  compare (IAttr (i, _)) (IAttr (i', _)) = compare i i'

data Color =
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Show, Eq)

data Style =
    Standout
  | Underline
  | ReverseVideo
  | Blink
  | Dim
  | Bold
  deriving (Show, Eq)

data Attr =
    FG Color
  | BG Color
  | ST Style
  --Attr (Maybe FG,   Maybe BG,   Maybe ST)
  | Attr (Maybe Attr, Maybe Attr, Maybe Attr)
  deriving (Show, Eq)

instance Default Attr where
  def = Attr (Nothing, Nothing, Nothing)

instance Monoid Attr where
  FG _ `mappend` FG fc = FG fc
  BG _ `mappend` BG bc = BG bc
  ST _ `mappend` ST s = ST s

  BG bc `mappend` FG fc = Attr (Just $ FG fc, Just $ BG bc, Nothing)
  FG fc `mappend` BG bc = Attr (Just $ FG fc, Just $ BG bc, Nothing)
  FG fc `mappend` ST s = Attr (Just $ FG fc, Nothing, Just $ ST s)
  ST s `mappend` FG fc = Attr (Just $ FG fc, Nothing, Just $ ST s)
  BG bc `mappend` ST s = Attr (Nothing, Just $ BG bc, Just $ ST s)
  ST s `mappend` BG bc = Attr (Nothing, Just $ BG bc, Just $ ST s)

  FG fc `mappend` Attr (f, b, s) = Attr (f <|> Just (FG fc), b, s)
  BG bc `mappend` Attr (f, b, s) = Attr (f, b <|> Just (BG bc), s)
  ST st `mappend` Attr (f, b, s) = Attr (f, b, s <|> Just (ST st))

  Attr (_, b, s) `mappend` FG fc = Attr (Just (FG fc), b, s)
  Attr (f, _, s) `mappend` BG bc = Attr (f, Just (BG bc), s)
  Attr (f, b, _) `mappend` ST s = Attr (f, b, Just (ST s))

  Attr (a, b, c) `mappend` Attr (a', b', c') = Attr (a' <|> a, b' <|> b, c' <|> c)

  mempty = Attr (Nothing, Nothing, Nothing)

fg, bg :: Color -> Attr
fg = FG
bg = BG

style :: Style -> Attr
style = ST
