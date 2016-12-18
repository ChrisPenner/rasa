{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables, DeriveFunctor, OverloadedStrings #-}
module Rasa.Utils where

import Control.Lens
import Control.Monad.Reader
import Data.List
import qualified Data.Text as T
import qualified Control.Lens.Text as TL
import Data.Monoid

-- | (Coord Row Column) represents a char in a block of text. (zero indexed)
-- e.g. Coord 0 0 is the first character in the text,
-- Coord 2 1 is the second character of the third row
data Coord =
  Coord Int
        Int
        deriving (Show, Eq)

instance Ord Coord where
  Coord a b <= Coord a' b'
    | a < a' = True
    | a > a' = False
    | otherwise = b <= b'

-- | A span applies some Monoid over the given range from start up to (not including) end
data Span a = Span
  { _start :: Coord
  , _end :: Coord
  , _data :: a
  } deriving (Show, Eq, Functor)

data Marker =
      Start
    | End
    deriving (Show, Eq)

-- | Combines a list of spans containing some monoidal data into a list of coords with
-- with the data that applies from each Coord forwards
combineSpans :: forall a. Monoid a =>  a -> [Span a] -> [(Coord, a)]
combineSpans base spans = combiner [(0, base)] $ sortOn (view _3) (splitStartEnd idSpans)
  where
    idSpans :: [(Int, Span a)]
    idSpans = zip [1..] spans

    splitStartEnd :: [(Int, Span a)] -> [(Marker, Int, Coord, a)]
    splitStartEnd [] = []
    splitStartEnd ((i, Span s e d):rest) = (Start, i, s, d):(End, i, e, d):splitStartEnd rest

    withoutId :: Int -> [(Int, a)] -> [(Int, a)]
    withoutId i = filter ((/=i).fst)

    combiner :: [(Int, a)] -> [(Marker, Int, Coord, a)] -> [(Coord, a)]
    combiner _ [] = []
    combiner cur ((Start, i, coord, mData):rest) =
      let dataSum = foldMap snd cur <> mData
          newData = (i, mData):cur
       in (coord, dataSum):combiner newData rest

    combiner cur ((End, i, coord, _):rest) =
      let dataSum = foldMap snd newData
          newData = withoutId i cur
       in (coord, dataSum):combiner newData rest


asCoord :: T.Text -> Iso' Int Coord
asCoord txt = iso (toCoord txt) (toOffset txt)


toOffset :: T.Text -> Coord -> Int
toOffset t (Coord row col) = clamp 0 (T.length t) $ rowCount + clamp 0 rowLen col
  where
    rowCount = t ^. TL.intillNextN row "\n" . to T.length
    rowLen = T.length $ T.lines t ^. ix row

toCoord :: T.Text -> Int -> Coord
toCoord txt offset = flip runReader txt $ do
  row <- view $ TL.before offset . TL.matching "\n" . to T.length
  col <-
    case row of
      0 -> return offset
      _ -> view $ TL.before offset . TL.tillPrev "\n" . to T.length
  return $ Coord row col

clamp :: Int -> Int -> Int -> Int
clamp mn mx n
  | n < mn = mn
  | n > mx = mx
  | otherwise = n

addCoord :: Coord -> Coord -> Coord
addCoord (Coord a b) (Coord a' b') = Coord (a+a') (b+b')
