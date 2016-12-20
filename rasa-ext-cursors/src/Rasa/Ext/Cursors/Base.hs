{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors.Base
  ( moveCoordsBy
  , moveCoordsBy'
  , moveCoordsTo
  , moveCoordsTo'
  , moveOffsetsBy
  , moveOffsetsBy'
  , moveOffsetsTo
  , moveOffsetsTo'
  , coordsDo
  , coordsDo_
  , offsetsDo
  , offsetsDo_
  , displayCursor
  ) where


import Rasa.Ext
import Rasa.Ext.Scheduler
import Rasa.Ext.Directive
import Rasa.Ext.Style

import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.List
import Data.Default
import qualified Yi.Rope as Y

import Rasa.Ext.Cursors.Types

newtype Cursor = Cursor
  { _cursors :: [Coord]
  } deriving (Show, Typeable)

makeLenses ''Cursor

instance Default Cursor where
  def = Cursor {
  _cursors=[Coord 1 0, Coord 0 0]
}

cleanCursors :: Y.YiString -> [Coord] -> [Coord]
cleanCursors txt = fmap (clampCoord txt) . reverse . nub . sort

coords :: Lens' Buffer [Coord]
coords = lens getter setter
  where getter buf = buf^.bufExt.cursors
        setter buf new = let txt = buf^.rope
                          in buf & bufExt.cursors .~ cleanCursors txt new


offsets :: Lens' Buffer [Int]
offsets = lens getter setter
  where getter buf = let txt = buf^.rope
                      in buf^..bufExt.cursors.to sort.to nub.reversed.traverse.from (asCoord txt)
        setter buf new = let txt = buf^.rope
                          in buf & bufExt.cursors .~ cleanCursors txt (view (asCoord txt) <$> new)

eachCoord :: Traversal' Buffer Coord
eachCoord = coords.traverse

eachOffset :: Traversal' Buffer Int
eachOffset = offsets.traverse

offsetsDo :: (Int -> BufAction a) -> BufAction [a]
offsetsDo f = use offsets >>= mapM f

offsetsDo_ :: (Int -> BufAction a) -> BufAction ()
offsetsDo_ = void . offsetsDo

coordsDo :: (Coord -> BufAction a) -> BufAction [a]
coordsDo f = use coords >>= mapM f

coordsDo_ :: (Coord -> BufAction a) -> BufAction ()
coordsDo_ = void . coordsDo

moveCoordsTo :: (Coord -> BufAction Coord) -> BufAction ()
moveCoordsTo f = coordsDo f >>= assign coords

moveCoordsTo' :: Coord -> BufAction ()
moveCoordsTo' c = eachCoord .= c

moveCoordsBy :: (Coord -> BufAction Coord) -> BufAction ()
moveCoordsBy f = do
  newCoords <- coordsDo f
  coords.partsOf each %= zipWith addCoord newCoords

moveCoordsBy' :: Coord -> BufAction ()
moveCoordsBy' c = eachCoord %= addCoord c

moveOffsetsBy :: (Int -> BufAction Int) -> BufAction ()
moveOffsetsBy f = do
  newOffsets <- offsetsDo f
  offsets.partsOf each %= zipWith (+) newOffsets

moveOffsetsBy' :: Int -> BufAction ()
moveOffsetsBy' o = eachOffset %= (+o)

moveOffsetsTo :: (Int -> BufAction Int) -> BufAction ()
moveOffsetsTo f = offsetsDo f >>= assign offsets

moveOffsetsTo' :: Int -> BufAction ()
moveOffsetsTo' o = offsets .= [o]

displayCursor ::  BufAction ()
displayCursor = offsetsDo_ setStyle
  where
    setStyle :: Int -> BufAction ()
    setStyle o = addStyle $ Span o (o+1) (flair ReverseVideo)
