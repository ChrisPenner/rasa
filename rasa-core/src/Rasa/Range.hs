{-# LANGUAGE Rank2Types, OverloadedStrings, DeriveFunctor, ScopedTypeVariables #-}
module Rasa.Range
  ( Coord(..)
  , Offset(..)
  , asCoord
  , asOffsets
  , clampCoord
  , clampRange
  , Range(..)
  , range
  , moveRange
  , moveRangeByN
  , moveCursorByN
  , moveCursorByC
  , Span(..)
  , combineSpans
  , clamp
  ) where

import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.List
import qualified Yi.Rope as Y


data Range =
  Range Coord Coord
  deriving (Show, Eq)

instance Ord Range where
  Range start end <= Range start' end' 
    | end == end' = start <= start'
    | otherwise = end <= end'

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

newtype Offset =
  Offset Int
  deriving (Show, Eq)

asOffsets :: Y.YiString -> Range -> (Offset, Offset)
asOffsets txt (Range start end) = (start^.from (asCoord txt), end^.from (asCoord txt))

moveRange :: Coord -> Range -> Range
moveRange amt (Range start end) =
  Range (moveCursorByC amt start) (moveCursorByC amt end)

moveRangeByN :: Int -> Range -> Range
moveRangeByN amt (Range start end) =
  Range (moveCursorByN amt start) (moveCursorByN amt end)

moveCursorByN :: Int -> Coord -> Coord
moveCursorByN amt (Coord row col) = Coord row (col + amt)

moveCursorByC :: Coord -> Coord -> Coord
moveCursorByC (Coord row col) (Coord row' col') = Coord (row + row') (col + col')

instance Num Coord where
  Coord row col + Coord row' col' = Coord (row + row') (col + col')
  Coord row col - Coord row' col' = Coord (row - row') (col - col')
  Coord row col * Coord row' col' = Coord (row * row') (col * col')
  abs (Coord row col) = Coord (abs row) (abs col)
  fromInteger i = Coord 0 (fromInteger i)
  signum (Coord row col) = Coord (signum row) (signum col)

range :: Range -> Lens' Y.YiString  Y.YiString
range r = lens getter setter
  where
    getter txt =
      let (Offset start, Offset end) = asOffsets txt r
       in Y.drop start . Y.take end $ txt
    setter old new =
      let (Offset start, Offset end) = asOffsets old r
          prefix = Y.take start old
          suffix = Y.drop end old
       in prefix <> new <> suffix

asCoord :: Y.YiString -> Iso' Offset Coord
asCoord txt = iso (toCoord txt) (toOffset txt)

toOffset :: Y.YiString -> Coord -> Offset
toOffset txt (Coord row col) = Offset $ lenRows + col
  where
    lenRows = Y.length . Y.concat . take row . Y.lines' $ txt

toCoord :: Y.YiString -> Offset -> Coord
toCoord txt (Offset offset) = Coord numRows numColumns
  where
    numRows = Y.countNewLines . Y.take offset $ txt
    numColumns = (offset -) . Y.length . Y.concat . take numRows . Y.lines' $ txt


clampCoord :: Y.YiString -> Coord -> Coord
clampCoord txt (Coord row col) =
  Coord (clamp 0 maxRow row) (clamp 0 maxColumn col)
  where
    maxRow = Y.countNewLines txt
    maxColumn = fromMaybe col (txt ^? to Y.lines' . ix row . to Y.length)


clampRange :: Y.YiString -> Range -> Range
clampRange txt (Range start end) =
  Range (clampCoord txt start) (clampCoord txt end)

-- | A span applies some Monoid over the given range from start up to (not including) end
data Span a = Span
  { _start :: Int
  , _end :: Int
  , _data :: a
  } deriving (Show, Eq, Functor)

data Marker
  = Start
  | End
  deriving (Show, Eq)

-- | Combines a list of spans containing some monoidal data into a list of offsets with
-- with the data that applies from each Offset forwards
combineSpans
  :: forall a.
     Monoid a
  => [Span a] -> [(Int, a)]
combineSpans spans = combiner [] $ sortOn (view _3) (splitStartEnd idSpans)
  where
    idSpans :: [(Int, Span a)]
    idSpans = zip [1 ..] spans

    splitStartEnd :: [(Int, Span a)] -> [(Marker, Int, Int, a)]
    splitStartEnd [] = []
    splitStartEnd ((i, Span s e d):rest) =
      (Start, i, s, d) : (End, i, e, d) : splitStartEnd rest

    withoutId :: Int -> [(Int, a)] -> [(Int, a)]
    withoutId i = filter ((/= i) . fst)

    combiner :: [(Int, a)] -> [(Marker, Int, Int, a)] -> [(Int, a)]
    combiner _ [] = []
    combiner cur ((Start, i, offset, mData):rest) =
      let dataSum = foldMap snd cur <> mData
          newData = (i, mData) : cur
      in (offset, dataSum) : combiner newData rest
    combiner cur ((End, i, offset, _):rest) =
      let dataSum = foldMap snd newData
          newData = withoutId i cur
      in (offset, dataSum) : combiner newData rest

clamp :: Int -> Int -> Int -> Int
clamp mn mx n
  | n < mn = mn
  | n > mx = mx
  | otherwise = n
