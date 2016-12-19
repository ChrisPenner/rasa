{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables, DeriveFunctor, OverloadedStrings #-}
module Rasa.Utils where

import Control.Lens
import Data.List
import Data.Monoid

-- | A span applies some Monoid over the given range from start up to (not including) end
data Span a = Span
  { _start :: Int
  , _end :: Int
  , _data :: a
  } deriving (Show, Eq, Functor)

data Marker =
      Start
    | End
    deriving (Show, Eq)

-- | Combines a list of spans containing some monoidal data into a list of offsets with
-- with the data that applies from each Offset forwards
combineSpans :: forall a. Monoid a =>  [Span a] -> [(Int, a)]
combineSpans spans = combiner [] $ sortOn (view _3) (splitStartEnd idSpans)
  where
    idSpans :: [(Int, Span a)]
    idSpans = zip [1..] spans

    splitStartEnd :: [(Int, Span a)] -> [(Marker, Int, Int, a)]
    splitStartEnd [] = []
    splitStartEnd ((i, Span s e d):rest) = (Start, i, s, d):(End, i, e, d):splitStartEnd rest

    withoutId :: Int -> [(Int, a)] -> [(Int, a)]
    withoutId i = filter ((/=i).fst)

    combiner :: [(Int, a)] -> [(Marker, Int, Int, a)] -> [(Int, a)]
    combiner _ [] = []
    combiner cur ((Start, i, offset, mData):rest) =
      let dataSum = foldMap snd cur <> mData
          newData = (i, mData):cur
       in (offset, dataSum):combiner newData rest

    combiner cur ((End, i, offset, _):rest) =
      let dataSum = foldMap snd newData
          newData = withoutId i cur
       in (offset, dataSum):combiner newData rest

clamp :: Int -> Int -> Int -> Int
clamp mn mx n
  | n < mn = mn
  | n > mx = mx
  | otherwise = n
