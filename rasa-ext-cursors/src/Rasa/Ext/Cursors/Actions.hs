module Rasa.Ext.Cursors.Actions
  ( delete
  , insertText
  , findNext
  , findPrev
  , findNextFrom
  , findPrevFrom
  , moveRangesByN
  , moveRangesByC
  ) where

import qualified Data.Text as T

import Control.Lens
import Control.Lens.Text
import Rasa.Ext
import Rasa.Ext.Directive
import Rasa.Ext.Cursors.Base

moveSameLineRangesBy :: Range -> Cursor -> BufAction ()
moveSameLineRangesBy (Range _ end) amt = do
  txt <- use rope
  let Coord endRow endCol = cursorToCoord txt end
      moveInLine r@(Range s _) = do
        let Coord startRow startCol = cursorToCoord txt s
        if endRow == startRow && startCol > endCol
            then moveRange amt r
            else return r
  ranges <~ rangeDo moveInLine

delete :: BufAction ()
delete = rangeDo_ $ \r -> do
  deleteRange r
  amt <- rangeSize r
  moveSameLineRangesBy r (Left . Offset $ (-amt))

insertText :: T.Text -> BufAction ()
insertText txt = rangeDo_ $ \r@(Range s _) -> do
  insertAt s txt
  moveSameLineRangesBy r (Left . Offset . T.length $ txt)

findNext :: T.Text -> BufAction ()
findNext pat = do
  res <- rangeDo $ \(Range _ e) -> do
    off <- findNextFrom pat e
    end <- moveCursorByN 1 off
    return $ Range off end
  ranges .= res

findNextFrom :: T.Text -> Cursor -> BufAction Cursor
findNextFrom pat c = do
  txt <- use rope
  let Offset o = cursorToOffset txt c
  distance <- use (text . after o . tillNext pat . to T.length)
  return (Left . Offset $ distance + o)

findPrev :: T.Text -> BufAction ()
findPrev pat = do
  res <- rangeDo $ \(Range _ e) -> do
    off <- findPrevFrom pat e
    end <- moveCursorByN 1 off
    return $ Range off end
  ranges .= res

findPrevFrom :: T.Text -> Cursor -> BufAction Cursor
findPrevFrom pat c = do
  txt <- use rope
  let Offset o = cursorToOffset txt c
  distance <- use (text . before o . tillPrev pat . to T.length .to negate)
  return (Left . Offset $ distance + o)

moveRangesByN :: Int -> BufAction ()
moveRangesByN n = overRanges $ moveRangeByN n

moveRangesByC :: Coord -> BufAction ()
moveRangesByC c = overRanges $ moveRangeByC c
