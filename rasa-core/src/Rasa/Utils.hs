{-# language ExistentialQuantification, ScopedTypeVariables #-}
module Rasa.Utils where

import Control.Lens
import Data.List
import Data.Monoid

data Coord =
  Coord Int
        Int
        deriving (Show, Eq)

instance Ord Coord where
  Coord a b <= Coord a' b'
    | a < a' = True
    | a > a' = False
    | otherwise = b <= b'

-- | Set of span objects
data Span a = Span
  { _start :: Coord
  , _end :: Coord
  , _data :: a
  } deriving (Show, Eq)

data Marker a =
  Start a
    | End a
    deriving Eq

instance (Ord a) => Ord (Marker a) where
  Start a `compare` Start b = a `compare` b
  Start a `compare` End b = a `compare` b
  End a `compare` Start b = a `compare` b
  End a `compare` End b = a `compare` b

extractMarker :: Marker a -> a
extractMarker (Start a) = a
extractMarker (End a) = a

combineSpans :: forall a. Monoid a =>  [Span a] -> [(Coord, a)]
combineSpans spans = combiner [] $ sortOn (view _2.extractMarker) (splitStartEnd idSpans)
  where 
    idSpans :: [(Int, Span a)]
    idSpans = zip [0..] spans

    splitStartEnd :: [(Int, Span a)] -> [Marker (Int, Coord, a)]
    splitStartEnd [] = []
    splitStartEnd ((i, Span s e d):rest) = Start (i, s, d):End (i, e, d):splitStartEnd rest

    combiner :: [(Int, a)] -> [Marker (Int, Coord, a)] -> [(Coord, a)]
    combiner _ [] = []
    combiner cur (Start (i, c, d):rest) = (c, foldMap snd cur <> d):combiner ((i, d):cur) rest
    combiner cur (End (i, c, _):rest) = (c, foldMap snd . withoutId i $ cur):combiner (withoutId i cur) rest
    withoutId i = filter ((/=i).fst)
