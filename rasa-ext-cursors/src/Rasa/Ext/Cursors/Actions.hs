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

moveSameLineRangesBy :: Range -> Coord -> BufAction ()
moveSameLineRangesBy (Range _ (Coord endRow endCol)) amt = do
  let moveInLine r@(Range (Coord startRow startCol) _) = return $
        if endRow == startRow && startCol > endCol
           then moveRange amt r
           else r
  ranges <~ rangeDo moveInLine

delete :: BufAction ()
delete = rangeDo_ $ \r -> do
  deleteRange r
  amt <- rangeSize r
  moveSameLineRangesBy r (Coord 0 (-amt))

insertText :: T.Text -> BufAction ()
insertText txt = rangeDo_ $ \r@(Range s _) -> do
  insertAt s txt
  moveSameLineRangesBy r (Coord 0 (T.length txt))

findNext :: T.Text -> BufAction ()
findNext pat = do
  res <- rangeDo $ \(Range _ e) -> do
    off <- findNextFrom pat e
    let end = moveCursorByN 1 off
    return $ Range off end
  ranges .= res

findNextFrom :: T.Text -> Coord -> BufAction Coord
findNextFrom pat c = do
  txt <- use rope
  let Offset o = c^.from (asCoord txt)
  distance <- use (text . after o . tillNext pat . to T.length)
  return ((Offset $ distance + o)^.asCoord txt)

findPrev :: T.Text -> BufAction ()
findPrev pat = do
  res <- rangeDo $ \(Range _ e) -> do
    off <- findPrevFrom pat e
    let end = moveCursorByN 1 off
    return $ Range off end
  ranges .= res

findPrevFrom :: T.Text -> Coord -> BufAction Coord
findPrevFrom pat c = do
  txt <- use rope
  let Offset o = c^.from (asCoord txt)
  distance <- use (text . before o . tillPrev pat . to T.length .to negate)
  return ((Offset $ distance + o)^.asCoord txt)

moveRangesByN :: Int -> BufAction ()
moveRangesByN n = overRanges $ return . moveRangeByN n

moveRangesByC :: Coord -> BufAction ()
moveRangesByC c = overRanges $ return . moveRange c
