{-# LANGUAGE Rank2Types, OverloadedStrings, DeriveFunctor, ScopedTypeVariables #-}
module Rasa.Range
  ( Coord(..)
  , Offset(..)
  , Cursor
  , asCoord
  , asOffsets
  , cursorToOffset
  , cursorToCoord
  , asCoords
  , addCoord
  , clampCoord
  , clampRange
  , Range(..)
  , range
  , moveRange
  , moveRange'
  , moveRangeByN
  , moveRangeByN'
  , moveRangeByC
  , moveRangeByC'
  , moveCursor
  , moveCursor'
  , moveCursorByN
  , moveCursorByN'
  , moveCursorByC
  , moveCursorByC'
  , Span(..)
  , combineSpans
  , clamp
  ) where

import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.List
import Rasa.Action
import Rasa.Buffer
import qualified Yi.Rope as Y


data Range =
  Range Cursor Cursor
  deriving (Show, Eq)

type Cursor = Either Offset Coord

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

cursorToCoord :: Y.YiString -> Cursor -> Coord
cursorToCoord txt (Left o) = o^.asCoord txt
cursorToCoord _ (Right c) = c

cursorToOffset :: Y.YiString -> Cursor -> Offset
cursorToOffset _ (Left o) = o
cursorToOffset txt (Right c) = c^.from (asCoord txt)

asCoords :: Y.YiString -> Range -> (Coord, Coord)
asCoords txt (Range start end) = (cursorToCoord txt start, cursorToCoord txt end)

asOffsets :: Y.YiString -> Range -> (Offset, Offset)
asOffsets txt (Range start end) = (cursorToOffset txt start, cursorToOffset txt end)

moveRange :: Cursor -> Range -> BufAction Range
moveRange amt r = do
  txt <- use rope
  return $ moveRange' txt amt r

moveRange' :: Y.YiString -> Cursor -> Range -> Range
moveRange' txt amt (Range start end) =
  Range (moveCursor' txt amt start) (moveCursor' txt amt end)

moveRangeByN :: Int -> Range -> BufAction Range
moveRangeByN amt r = do
  txt <- use rope
  return $ moveRange' txt (Left . Offset $ amt) r

moveRangeByN' :: Y.YiString -> Int -> Range -> Range
moveRangeByN' txt amt = moveRange' txt (Left . Offset $ amt)

moveRangeByC :: Coord -> Range -> BufAction Range
moveRangeByC amt r = do
  txt <- use rope
  return $ moveRange' txt (Right amt) r

moveRangeByC' :: Y.YiString -> Coord -> Range -> Range
moveRangeByC' txt amt = moveRange' txt (Right amt)

moveCursor' :: Y.YiString -> Cursor -> Cursor -> Cursor
moveCursor' txt amt cur = let Offset amt' = cursorToOffset txt amt
                           in moveCursorByN' txt amt' cur

moveCursorByN :: Int -> Cursor -> BufAction Cursor
moveCursorByN amt cur = do
  txt <- use rope
  return $ moveCursorByN' txt amt cur

moveCursorByN' :: Y.YiString -> Int -> Cursor -> Cursor
moveCursorByN' txt amt cur = let Offset cur' = cursorToOffset txt cur
                              in Left . Offset $ amt + cur'

moveCursorByC :: Coord -> Cursor -> BufAction Cursor
moveCursorByC amt = moveCursor (Right amt)

moveCursorByC' :: Y.YiString -> Coord -> Cursor -> Cursor
moveCursorByC' txt amt = moveCursor' txt (Right amt)

moveCursor :: Cursor -> Cursor -> BufAction Cursor
moveCursor amt cur = do
  txt <- use rope
  return $ moveCursor' txt amt cur

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

addCoord :: Coord -> Coord -> Coord
addCoord (Coord a b) (Coord a' b') = Coord (a + a') (b + b')

clampCoord :: Y.YiString -> Coord -> Coord
clampCoord "" _ = Coord 0 0
clampCoord txt (Coord row col) =
  Coord (clamp 0 maxRow row) (clamp 0 maxColumn col)
  where
    maxRow = Y.countNewLines txt
    maxColumn = fromMaybe col (txt ^? to Y.lines' . ix row . to Y.length)


clampRange :: Y.YiString -> Range -> Range
clampRange txt r =
  Range (Left . Offset $ clamp 0 mx start) (Left . Offset $ clamp 0 mx end)
  where
    mx = Y.length txt
    (Offset start, Offset end) = asOffsets txt r


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
