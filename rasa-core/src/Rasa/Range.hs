{-# LANGUAGE Rank2Types, OverloadedStrings, DeriveFunctor, ScopedTypeVariables #-}
module Rasa.Range
  ( Coord(..)
  , Offset(..)
  , asCoord
  , clampCoord
  , clampRange
  , Range(..)
  , range
  , sizeOf
  , sizeOfR
  , moveRange
  , moveRangeByN
  , moveCursorByN
  , moveCursor
  , Span(..)
  , combineSpans
  , clamp
  , beforeC
  , afterC
  ) where

import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.List
import Rasa.Text
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

moveRange :: Coord -> Range -> Range
moveRange amt (Range start end) =
  Range (moveCursor amt start) (moveCursor amt end)

moveRangeByN :: Int -> Range -> Range
moveRangeByN amt (Range start end) =
  Range (moveCursorByN amt start) (moveCursorByN amt end)

moveCursorByN :: Int -> Coord -> Coord
moveCursorByN amt (Coord row col) = Coord row (col + amt)

moveCursor :: Coord -> Coord -> Coord
moveCursor (Coord row col) (Coord row' col') = Coord (row + row') (col + col')

instance Num Coord where
  Coord row col + Coord row' col' = Coord (row + row') (col + col')
  Coord row col - Coord row' col' = Coord (row - row') (col - col')
  Coord row col * Coord row' col' = Coord (row * row') (col * col')
  abs (Coord row col) = Coord (abs row) (abs col)
  fromInteger i = Coord 0 (fromInteger i)
  signum (Coord row col) = Coord (signum row) (signum col)

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

-- | A span applies some Monoid over the given range.
data Span a = Span
  { _getRange :: Range
  , _data :: a
  } deriving (Show, Eq, Functor)

data Marker
  = Start
  | End
  deriving (Show, Eq)

type ID = Int
-- | Combines a list of spans containing some monoidal data into a list of offsets with
-- with the data that applies from each Offset forwards
combineSpans
  :: forall a.
     Monoid a
  => [Span a] -> [(Coord, a)]
combineSpans spans = combiner [] $ sortOn (view _3) (splitStartEnd idSpans)
  where
    idSpans :: [(ID, Span a)]
    idSpans = zip [1 ..] spans

    splitStartEnd :: [(ID, Span a)] -> [(Marker, ID, Coord, a)]
    splitStartEnd [] = []
    splitStartEnd ((i, Span (Range s e) d):rest) =
      (Start, i, s, d) : (End, i, e, d) : splitStartEnd rest

    withoutId :: ID -> [(ID, a)] -> [(ID, a)]
    withoutId i = filter ((/= i) . fst)

    combiner :: [(ID, a)] -> [(Marker, ID, Coord, a)] -> [(Coord, a)]
    combiner _ [] = []
    combiner cur ((Start, i, crd, mData):rest) =
      let dataSum = foldMap snd cur <> mData
          newData = (i, mData) : cur
      in (crd, dataSum) : combiner newData rest
    combiner cur ((End, i, crd, _):rest) =
      let dataSum = foldMap snd newData
          newData = withoutId i cur
      in (crd, dataSum) : combiner newData rest

clamp :: Int -> Int -> Int -> Int
clamp mn mx n
  | n < mn = mn
  | n > mx = mx
  | otherwise = n

sizeOfR :: Range -> Coord
sizeOfR (Range start end) = end - start

sizeOf :: Y.YiString -> Coord
sizeOf txt = Coord (Y.countNewLines txt) (Y.length (txt ^. asLines . _last))

range :: Range -> Lens' Y.YiString Y.YiString
range (Range start end) = lens getter setter
  where getter = view (beforeC end . afterC start)
        setter old new = old & beforeC end . afterC start .~ new


beforeC :: Coord -> Lens' Y.YiString  Y.YiString
beforeC c@(Coord row col) = lens getter setter
  where getter txt =
          txt ^.. asLines . taking (row + 1) traverse
              & _last %~ Y.take col
              & Y.unlines

        setter old new = let suffix = old ^. afterC c
                          in new <> suffix

afterC :: Coord -> Lens' Y.YiString  Y.YiString
afterC c@(Coord row col) = lens getter setter
  where getter txt =
          txt ^.. asLines . dropping row traverse
              & _head %~ Y.drop col
              & Y.unlines

        setter old new = let prefix = old ^. beforeC c
                          in prefix <> new
