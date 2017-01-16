{-# LANGUAGE
  Rank2Types
  , OverloadedStrings
  , DeriveFunctor
  , ScopedTypeVariables
  , TemplateHaskell 
#-}
module Rasa.Internal.Range
  ( Coord
  , Coord'(..)
  , overRow
  , overCol
  , overBoth
  , coordRow
  , coordCol
  , Offset(..)
  , asCoord
  , clampCoord
  , clampRange
  , Range(..)
  , CrdRange
  , range
  , rStart
  , rEnd
  , getRange
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

import Rasa.Internal.Buffer
import Rasa.Internal.Text
import Control.Lens

import Data.Maybe
import Data.Monoid
import Data.List
import Data.Bifunctor
import Data.Biapplicative
import Data.Bitraversable
import Data.Bifoldable

import qualified Yi.Rope as Y

-- | This represents a range between two coordinates ('Coord')
data Range a b = Range
  { _rStart :: a
  , _rEnd :: b
  } deriving (Show, Eq)
makeLenses ''Range

instance Bifunctor Range where
  bimap f g (Range a b) = Range (f a) (g b)

instance Bifoldable Range where
  bifoldMap f g (Range a b) = f a `mappend` g b

instance Bitraversable Range where
  bitraverse f g (Range a b) = Range <$> f a <*> g b

instance (Ord a, Ord b) => Ord (Range a b) where
  Range start end <= Range start' end'
    | end == end' = start <= start'
    | otherwise = end <= end'

-- | (Coord Row Column) represents a char in a block of text. (zero indexed)
-- e.g. Coord 0 0 is the first character in the text,
-- Coord 2 1 is the second character of the third row
data Coord' a b = Coord 
  { _coordRow::a
  , _coordCol::b
  } deriving (Show, Eq)
makeLenses ''Coord'
type Coord = Coord' Int Int
type CrdRange = Range Coord Coord

instance Bifunctor Coord' where
  bimap f g (Coord a b) = Coord (f a) (g b)

overRow :: (Int -> Int) -> Coord -> Coord
overRow = first

overCol :: (Int -> Int) -> Coord -> Coord
overCol = second

overBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
overBoth f = bimap f f

instance Biapplicative Coord' where
  bipure = Coord
  Coord f g <<*>> Coord a b = Coord (f a) (g b)

instance (Ord a, Ord b) => Ord (Coord' a b) where
  Coord a b <= Coord a' b'
    | a < a' = True
    | a > a' = False
    | otherwise = b <= b'

-- | An 'Offset' represents an exact position in a file as a number of characters from the start.
newtype Offset =
  Offset Int
  deriving (Show, Eq)

-- | A span which maps a piece of Monoidal data over a range.
data Span a b =
  Span a b
  deriving (Show, Eq, Functor)

instance Bifunctor Span where
  bimap f g (Span a b) = Span (f a) (g b)

-- | Moves a 'Range' by a given 'Coord'
-- It may be unintuitive, but for (Coord row col) a given range will be moved down by row and to the right by col.
moveRange :: Coord -> CrdRange -> CrdRange
moveRange amt = overBoth (moveCursor amt)

-- | Moves a range forward by the given amount
moveRangeByN :: Int -> CrdRange -> CrdRange
moveRangeByN amt = overBoth (moveCursorByN amt)

-- | Moves a 'Coord' forward by the given amount of columns
moveCursorByN :: Int -> Coord -> Coord
moveCursorByN amt = overCol (+amt)

-- | Adds the rows and columns of the given two 'Coord's.
moveCursor :: Coord -> Coord -> Coord
moveCursor = biliftA2 (+) (+)

instance (Num a, Num b) => Num (Coord' a b) where
  Coord row col + Coord row' col' = Coord (row + row') (col + col')
  Coord row col - Coord row' col' = Coord (row - row') (col - col')
  Coord row col * Coord row' col' = Coord (row * row') (col * col')
  abs (Coord row col) = Coord (abs row) (abs col)
  fromInteger i = Coord 0 (fromInteger i)
  signum (Coord row col) = Coord (signum row) (signum col)

-- | Given the text you're operating over, creates an iso from an 'Offset' to a 'Coord'.
asCoord :: Y.YiString -> Iso' Offset Coord
asCoord txt = iso (toCoord txt) (toOffset txt)

-- | Given the text you're operating over, converts a 'Coord' to an 'Offset'.
toOffset :: Y.YiString -> Coord -> Offset
toOffset txt (Coord row col) = Offset $ lenRows + col
  where
    lenRows = Y.length . Y.concat . take row . Y.lines' $ txt

-- | Given the text you're operating over, converts an 'Offset' to a 'Coord'.
toCoord :: Y.YiString -> Offset -> Coord
toCoord txt (Offset offset) = Coord numRows numColumns
  where
    numRows = Y.countNewLines . Y.take offset $ txt
    numColumns = (offset -) . Y.length . Y.concat . take numRows . Y.lines' $ txt

-- | This will restrict a given 'Coord' to a valid one which lies within the given text.
clampCoord :: Y.YiString -> Coord -> Coord
clampCoord txt (Coord row col) =
  Coord (clamp 0 maxRow row) (clamp 0 maxColumn col)
  where
    maxRow = Y.countNewLines txt
    maxColumn = fromMaybe col (txt ^? to Y.lines' . ix row . to Y.length)

-- | This will restrict a given 'Range' to a valid one which lies within the given text.
clampRange :: Y.YiString -> CrdRange -> CrdRange
clampRange txt = overBoth (clampCoord txt)

-- | A Helper only used when combining many spans.
data Marker
  = Start
  | End
  deriving (Show, Eq)

type ID = Int
-- | Combines a list of spans containing some monoidal data into a list of offsets with
-- with the data that applies from each Offset forwards.
combineSpans
  :: forall a.
     Monoid a
    => [Span CrdRange a] -> [(Coord, a)]
combineSpans spans = combiner [] $ sortOn (view _3) (splitStartEnd idSpans)
  where
    idSpans :: [(ID, Span CrdRange a)]
    idSpans = zip [1 ..] spans

    splitStartEnd :: [(ID, Span CrdRange a)] -> [(Marker, ID, Coord, a)]
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

-- | @clamp min max val@ restricts val to be within min and max (inclusive)
clamp :: Int -> Int -> Int -> Int
clamp mn mx n
  | n < mn = mn
  | n > mx = mx
  | otherwise = n

-- | Returns the number of rows and columns that a 'Range' spans as a 'Coord'
sizeOfR :: CrdRange -> Coord
sizeOfR (Range start end) = end - start

-- | Returns the number of rows and columns that a chunk of text spans as a 'Coord'
sizeOf :: Y.YiString -> Coord
sizeOf txt = Coord (Y.countNewLines txt) (Y.length (txt ^. asLines . _last))

-- | A lens over text before a given 'Coord'
beforeC :: Coord -> Lens' Y.YiString  Y.YiString
beforeC c@(Coord row col) = lens getter setter
  where getter txt =
          txt ^.. asLines . taking (row + 1) traverse
              & _last %~ Y.take col
              & Y.unlines

        setter old new = let suffix = old ^. afterC c
                          in new <> suffix

-- | A lens over text after a given 'Coord'
afterC :: Coord -> Lens' Y.YiString  Y.YiString
afterC c@(Coord row col) = lens getter setter
  where getter txt =
          txt ^.. asLines . dropping row traverse
              & _head %~ Y.drop col
              & Y.unlines

        setter old new = let prefix = old ^. beforeC c
                          in prefix <> new

-- | A lens over text which is encompassed by a 'Range'
range :: HasBuffer s =>  CrdRange -> Lens' s Y.YiString
range (Range start end) = lens getter setter
  where getter = view (text . beforeC end . afterC start)
        setter old new = old & text . beforeC end . afterC start .~ new

-- | A getter-lens over text which is encompassed by a 'Range'
getRange :: HasBuffer s => CrdRange -> Getting r s Y.YiString
getRange (Range start end) = to getter
  where getter = view (text . beforeC end . afterC start)
