{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors
  ( moveCoordsBy
  , moveCoordsBy'
  , moveCoordsTo
  , moveOffsetsBy
  , moveOffsetsBy'
  , moveOffsetsTo
  , moveOffsetsTo'
  , cursorMain
  , deleteChar
  , insertText
  , findNext
  , findPrev
  , Coord(..)
  ) where

import Rasa.Ext
import Rasa.Ext.Cursors.Types
import Rasa.Ext.Scheduler
import Rasa.Ext.Directive
import Rasa.Ext.Style

import Control.Monad.State
import Control.Lens
import Control.Lens.Text
import Data.Typeable
import Data.List
import Data.Default

import qualified Data.Text as T
newtype Cursor = Cursor
  { _cursors :: [Coord]
  } deriving (Show, Typeable)

makeLenses ''Cursor

instance Default Cursor where
  def = Cursor {
  _cursors=[Coord 1 0, Coord 0 0]
}

-- clamped :: Y.YiString -> Lens' Coord Coord
-- clamped txt = lens getter setter
--   where
--     getter = id
--     setter _ = clampCoord txt


coords :: Lens' Buffer [Coord]
coords = lens getter setter
  where getter buf = buf^.bufExt.cursors.to sort.to nub.reversed
        setter buf new = let txt = buf^.rope
                          in buf & bufExt.cursors .~ (clampCoord txt <$> new)


offsets :: Lens' Buffer [Int]
offsets = lens getter setter
  where getter buf = let txt = buf^.rope
                      in buf^..bufExt.cursors.to sort.to nub.reversed.traverse.from (asCoord txt)
        setter buf new = let txt = buf^.rope
                          in buf & bufExt.cursors.partsOf each .~ (clampCoord txt.view (asCoord txt) <$> new)


aCoord :: Traversal' Buffer Coord
aCoord = coords.traverse

anOffset :: Traversal' Buffer Int
anOffset = offsets.traverse

-- offsets :: Traversal' Buffer Coord
-- offsets = let txt = buf^.text
--            in buf.bufExt.cursors.traverse.clamped txt.asCoord txt

displayCursor ::  BufAction ()
displayCursor = offsetsDo_ setStyle
  where
    setStyle :: Int -> BufAction ()
    setStyle o = addStyle $ Span o (o+1) (flair ReverseVideo)

cursorMain :: Scheduler ()
cursorMain = beforeRender $ bufDo displayCursor

offsetsDo :: (Int -> BufAction a) -> BufAction [a]
offsetsDo f = do
  o <- use offsets
  mapM f o

offsetsDo_ :: (Int -> BufAction a) -> BufAction ()
offsetsDo_ = void . offsetsDo

moveCoordsTo :: (Coord -> BufAction Coord) -> BufAction ()
moveCoordsTo f = do
  oldCoords <- use coords
  newCoords <- mapM f oldCoords
  coords.partsOf each .= newCoords

moveCoordsBy :: (Coord -> BufAction Coord) -> BufAction ()
moveCoordsBy f = do
  oldCoords <- use coords
  newCoords <- mapM f oldCoords
  coords.partsOf each .= zipWith addCoord oldCoords newCoords

moveCoordsBy' :: Coord -> BufAction ()
moveCoordsBy' c = aCoord %= addCoord c

moveOffsetsBy :: (Int -> BufAction Int) -> BufAction ()
moveOffsetsBy f = do
  oldOffsets <- use offsets
  newOffsets <- mapM f oldOffsets
  offsets.partsOf each .= zipWith (+) oldOffsets newOffsets

moveOffsetsBy' :: Int -> BufAction ()
moveOffsetsBy' o = anOffset %= (+o)

moveOffsetsTo :: (Int -> BufAction Int) -> BufAction ()
moveOffsetsTo f = do
  oldOffsets <- use offsets
  newOffsets <- mapM f oldOffsets
  offsets.partsOf each .= newOffsets

moveOffsetsTo' :: Int -> BufAction ()
moveOffsetsTo' o = offsets .= [o]


-- moveCursorTo :: Coord -> BufAction ()
-- moveCursorTo c = coord .= c

-- moveCursorBy :: Coord -> BufAction ()
-- moveCursorBy c = coord %= addCoord c

-- moveCursorOffsetBy :: Int -> BufAction ()
-- moveCursorOffsetBy i = offset %= (+i)

-- moveCursorOffsetTo :: Int -> BufAction ()
-- moveCursorOffsetTo i = offset .= i

deleteChar :: BufAction ()
deleteChar = offsetsDo_ deleteCharAt

insertText :: T.Text -> BufAction ()
insertText txt = offsetsDo_ $ insertTextAt txt


findNext :: T.Text -> BufAction ()
findNext txt = moveOffsetsBy distNext
  where
    distNext :: Int -> BufAction Int
    distNext o = use (text.after o.tillNext txt.to T.length)

findPrev :: T.Text -> BufAction ()
findPrev txt = moveOffsetsBy distPrev
  where
    distPrev :: Int -> BufAction Int
    distPrev o = use (text.before o.tillPrev txt.to T.length.to negate)

