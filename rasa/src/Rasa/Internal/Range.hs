{-# LANGUAGE
  Rank2Types
  , OverloadedStrings
  , DeriveFunctor
  , ScopedTypeVariables
  , TemplateHaskell
  , FlexibleInstances
  , MultiParamTypeClasses
#-}
module Rasa.Internal.Range
  ( Coord
  , Coord'(..)
  , overRow
  , overCol
  , overBoth
  , coordRow
  , coordCol
  , Pos
  , asCoord
  , clampCoord
  , clampRange
  , Range(..)
  , PosRange
  , CrdRange
  , range
  , rStart
  , rEnd
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
  , posSpans
  , fromSpanList
  , insertSpan
  , chop
  ) where

import Rasa.Internal.Text
import Control.Lens

import Data.Monoid
import Data.List
import Data.Bifunctor
import Data.Biapplicative
import Data.Bitraversable
import Data.Bifoldable


-- Look into http://hackage.haskell.org/package/fingertree-0.1.4.1/docs/Data-IntervalMap-FingerTree.html
import Data.IntervalMap.Generic.Interval (Interval(..), lowerBound, upperBound, rightClosed, overlaps, isEmpty)
import Data.IntervalMap.Generic.Strict (IntervalMap)
import qualified Data.IntervalMap as IM

import qualified Yi.Rope as Y

-- | This represents a position in the text
type Pos = Int

-- | This represents a range between two positions ('Pos')
data Range a b = Range
  { _rStart :: a
  , _rEnd :: b
  } deriving (Eq)
makeLenses ''Range

instance (Show a, Show b) => Show (Range a b) where
  show (Range a b) = "(Range (start " ++ show a ++ ") (end " ++ show b ++ "))"

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

-- | A type alias to 'Range'' which specializes the types to 'Pos's.
type PosRange = (Pos, Pos)

-- | A span which maps a piece of Monoidal data over a range.
data Span a b =
  Span a b
  deriving (Show, Eq, Functor)

instance Bifunctor Span where
  bimap f g (Span a b) = Span (f a) (g b)

-- | (Coord Row Column) represents a coordinate on screen. (zero indexed)
-- e.g. Coord 0 0 is the first character in the text,
-- Coord 2 1 is the second character of the third row
data Coord' a b = Coord
  { _coordRow::a
  , _coordCol::b
  } deriving (Eq)
makeLenses ''Coord'

instance (Show a, Show b) => Show (Coord' a b) where
  show (Coord a b) = "(Coord " ++ show a ++ " " ++ show b ++ ")"

-- | A type alias to 'Coord'' which specializes the types to integers.
type Coord = Coord' Int Int

-- | A type alias to 'Range'' which specializes the types to 'Coord's.
type CrdRange = Range Coord Coord

instance Bifunctor Coord' where
  bimap f g (Coord a b) = Coord (f a) (g b)

instance Biapplicative Coord' where
  bipure = Coord
  Coord f g <<*>> Coord a b = Coord (f a) (g b)

instance (Ord a, Ord b) => Ord (Coord' a b) where
  Coord a b <= Coord a' b'
    | a < a' = True
    | a > a' = False
    | otherwise = b <= b'

-- | Applies a function over the row of a 'Coord'
overRow :: (Int -> Int) -> Coord -> Coord
overRow = first

-- | Applies a function over the column of a 'Coord'
overCol :: (Int -> Int) -> Coord -> Coord
overCol = second

-- | Applies a function over both functors in any 'Bifunctor'.
overBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
overBoth f = bimap f f

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


-- Is there really an Iso given that some Coord's can be outside of a
-- text block?

-- | Given the text you're operating over, creates an iso from an 'Pos' to a 'Coord'.
asCoord :: Y.YiString -> Iso' Pos Coord
asCoord txt = iso (toCoord txt) (toPos txt)

-- | Given the text you're operating over, converts a 'Coord' to an 'Pos'.
toPos :: Y.YiString -> Coord -> Pos
toPos txt (Coord row col) = lenRows + col
  where
    lenRows = Y.length . Y.concat . take row . Y.lines' $ txt

-- | Given the text you're operating over, converts an 'Pos' to a 'Coord'.
toCoord :: Y.YiString -> Pos -> Coord
toCoord txt pos = Coord numRows numColumns
  where
    numRows = Y.countNewLines . Y.take pos $ txt
    numColumns = (pos -) . Y.length . Y.concat . take numRows . Y.lines' $ txt

-- | This will restrict a given 'Coord' to a valid one which lies within the given text.
clampCoord :: Y.YiString -> Coord -> Coord
clampCoord txt (Coord row col) =
  Coord (clamp 0 maxRow row) (clamp 0 maxColumn col)
  where
    l = Y.lines txt
    maxRow = length l
    selectedRow = maybe Y.empty id (l ^? element row)
    maxColumn = Y.length selectedRow

-- | This will restrict a given 'Range' to a valid one which lies within the given text.
clampRange :: Y.YiString -> CrdRange -> CrdRange
clampRange txt = overBoth (clampCoord txt)

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
  where getter txt = let (before, after) = Y.splitAtLine row $ txt
                      in before <> Y.take col after
        setter old new = let suffix = old ^. afterC c
                          in new <> suffix

-- | A lens over text after a given 'Coord'
afterC :: Coord -> Lens' Y.YiString  Y.YiString
afterC c@(Coord row col) = lens getter setter
  where getter txt = Y.drop col . snd . Y.splitAtLine row $ txt
        setter old new = let prefix = old ^. beforeC c
                          in prefix <> new

-- | A lens over text which is encompassed by a 'Range'
range :: CrdRange -> Lens' Y.YiString Y.YiString
range (Range start end) = lens getter setter
  where getter = view (beforeC (end + 1) . afterC start)
        setter old new = result
          where
            setBefore = old & beforeC (end + 1) .~ new
            result = old & afterC start .~ setBefore

instance Ord e => Interval (e, e) e where
    lowerBound (a,_) = a
    upperBound (_,b) = b
    rightClosed _ = False

-- Spans seem like they're more than a semigroup as it should be possible
-- to remove Spans from Spans. Like a Set, but slightly more because of the
-- intervals, which should allow to remove " a " from "aaa" to give "a a".
-- Possibly we want to allow to remove "aaa" from "a" as well to give "   ".
-- A bit like if there was a set for each given Coord.

type Spans a = IntervalMap PosRange a

-- | Combines a list of spans containing some monoidal data into a list of
-- | positions with the data that applies from each Pos forwards.
combineSpans :: (Monoid a) => Y.YiString -> [Span CrdRange a] -> [(Pos,a)]
combineSpans txt spans = concat [ steps spans' , [final spans'] ]
  where
    spans' = fromSpanList (map (posSpans txt) spans)

posSpans :: Y.YiString -> Span CrdRange a -> Span PosRange a
posSpans txt (Span (Range s e) a) = Span (toPos txt s, (toPos txt e) + 1) a

final :: Monoid a => Spans a -> (Pos, a)
final s
  | null s = (0, mempty)
  | otherwise = case IM.findMax s of
    (i,_) -> (upperBound i,mempty)

steps :: Spans a -> [(Pos, a)]
steps s = flip map (IM.toList s) $ \((s,_),a) -> (s,a)

--     -- A = aa
--     -- B =  b
--     --     ---
--     --     aa.
--     --      b
--
--     -- A = aaa
--     -- B =  b
--     --     ---
--     --     aaa.
--     --      b

fromSpanList :: (Monoid a) => [Span PosRange a] -> IntervalMap PosRange a
fromSpanList xs = foldl' ins IM.empty xs
  where
    ins :: (Monoid a) => IntervalMap PosRange a -> Span PosRange a -> IntervalMap PosRange a
    ins t (Span k x)
      | isEmpty k = t
      | otherwise = insertSpan k x t

-- Seems that this is introducing a new invariant (i.e no overlapping keys)
-- If that was enforced by the underlying data structure, it
-- would surely have better asymptotics... And maybe some commutativity law with
-- steps? i.e. steps (combineSpans [a]) = someOperation map steps [a]

insertSpan :: (Monoid v) => PosRange -> v -> IntervalMap PosRange v -> IntervalMap PosRange v
insertSpan key value (mp :: IntervalMap PosRange v)
  | null mp = IM.singleton key value
  | otherwise = case (IM.splitIntersecting mp key) of
    -- (_,empty:: IntervalMap PosRange v ,_) -> IM.insert key value mp
    (below, intersecting :: IntervalMap PosRange v , above) -> go intersecting
      where
        go i | null i    = IM.unions [ below, IM.singleton key value, above]
             | otherwise = IM.unions [ below
                                     , combine key value i
                                     , above ]
        -- Surely not the most efficient algorithm but sufficiently easy to reason about
        --  - split all values in the map with the upper and lower bounds of inserted value
        --  - split inserted value with all the intervals in the intersecting map
        --  - unionWith with <>
        combine :: (Semigroup v) => PosRange -> v -> IntervalMap PosRange v -> IntervalMap PosRange v
        combine key value mp = IM.unionWith
          (<>)
          ((IM.foldrWithKey (chop [key]) IM.empty mp) :: IntervalMap PosRange v)
          ((IM.foldrWithKey (chop (IM.keys mp)) IM.empty (IM.singleton key value)) :: IntervalMap PosRange v)
        --   |. .|   |. .|  |.|
        --  a a a a   b b  c c
        --  a|a a|a   b b  c|c

        -- foldrWithKey :: (k -> v -> a -> a) -> a -> IntervalMap k v -> a
chop :: (Monoid v) => [PosRange] -> PosRange -> v -> IntervalMap PosRange v -> IntervalMap PosRange v
chop ks key val acc = IM.union acc $ IM.unions $ concat splits
  where
    -- splits :: [[IntervalMap PosRange v]]
    splits = flip map ks $ \k ->
     flip map (splitIntervals k key) $ \j -> case overlaps j key of
         True -> IM.singleton j val
         False -> IM.empty

insertMultipleKeys :: [PosRange] -> v -> IntervalMap PosRange v -> IntervalMap PosRange v
insertMultipleKeys [] _ m = m
insertMultipleKeys (k:ks) v m = IM.insert k v (insertMultipleKeys ks v m)

splitIntervals :: PosRange -> PosRange -> [PosRange]
splitIntervals a b | a `overlaps` b = splitOverlappingIntervals a b
                   | otherwise      = [a,b]

-- This can surely be written in one line.
splitOverlappingIntervals :: PosRange -> PosRange -> [PosRange]
splitOverlappingIntervals a b = case (compare a b, compareUpperBounds a b) of
  -- aaaa
  --  bbbb
  (LT, LT) -> [ (lowerBound a, lowerBound b)
              , (lowerBound b, upperBound a)
              , (upperBound a, upperBound b)
              ]
  -- aaaa
  --  bbb
  (LT, EQ) -> [ (lowerBound a, lowerBound b)
              , (lowerBound b, upperBound a)
              ]
  -- aaaa
  --  bb
  (LT, GT) -> [ (lowerBound a, lowerBound b)
              , (lowerBound b, upperBound b)
              , (upperBound b, upperBound a)
              ]
  -- aa
  -- bbb
  (EQ, LT) -> [ (lowerBound a, upperBound a)
              , (upperBound a, upperBound b)
              ]
  -- aa
  -- bb
  (EQ, EQ ) -> [ a ]
  -- aaa
  -- bb
  (EQ, GT) -> [ (lowerBound a, lowerBound b)
              , (lowerBound b, upperBound a)
              ]
  --  aaa
  -- bbbbb
  (GT, LT) -> [ (lowerBound b, lowerBound a)
              , (lowerBound a, upperBound a)
              , (upperBound a, upperBound b)
              ]
  --  aaa
  -- bbbb
  (GT, EQ) -> [ (lowerBound b, lowerBound a)
              , (lowerBound a, upperBound a)
              ]
  --  aaaa
  -- bbbb
  (GT, GT) -> [ (lowerBound b, lowerBound a)
              , (lowerBound a, upperBound b)
              , (upperBound b, upperBound a)
              ]
