{-# language Rank2Types, OverloadedStrings #-}
module Rasa.Ext.Cursors.Types where

import Rasa.Ext
import Control.Lens
import Data.Maybe
import qualified Yi.Rope as Y

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

asCoord :: Y.YiString -> Iso' Int Coord
asCoord txt = iso (toCoord txt) (toOffset txt)

toOffset :: Y.YiString -> Coord -> Int
toOffset txt (Coord row col) = lenRows + col
  where lenRows = Y.length . Y.concat . take row . Y.lines' $ txt

toCoord :: Y.YiString -> Int -> Coord
toCoord txt offset = Coord numRows numColumns
  where numRows = Y.countNewLines . Y.take offset $ txt
        numColumns = (offset-) . Y.length . Y.concat . take numRows . Y.lines' $ txt

addCoord :: Coord -> Coord -> Coord
addCoord (Coord a b) (Coord a' b') = Coord (a+a') (b+b')

clampCoord ::  Y.YiString -> Coord -> Coord
clampCoord "" _ = Coord 0 0
clampCoord txt (Coord row col) = Coord (clamp 0 maxRow row) (clamp 0 maxColumn col)
  where
    maxRow = Y.countNewLines txt
    maxColumn = fromMaybe col (txt^?to Y.lines'.ix row.to Y.length)
