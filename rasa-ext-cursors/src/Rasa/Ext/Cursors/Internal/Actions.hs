module Rasa.Ext.Cursors.Internal.Actions
  ( delete
  , insertText
  , findNext
  , findPrev
  , findNextFrom
  , findPrevFrom
  , moveRangesByN
  , moveRangesByC
  ) where

import qualified Yi.Rope as Y

import Control.Lens
import Control.Lens.Text
import Rasa.Ext
import Rasa.Ext.Cursors.Internal.Base

-- | Moves all Ranges that are on the same END row as the given range by the coord's row and column
-- This is used to adjust cursors when things have been inserted/deleted before them in the row.
moveSameLineRangesBy :: CrdRange -> Coord -> BufAction ()
moveSameLineRangesBy (Range _ (Coord endRow endCol)) amt = do
  let moveInLine r@(Range (Coord startRow startCol) _) = return $
        if endRow == startRow && startCol > endCol
           then moveRange amt r
           else r
  rangeDo moveInLine >>= setRanges

-- | Delete the text of all ranges in a buffer
delete :: BufAction ()
delete = rangeDo_ $ \r -> do
  deleteRange r
  moveSameLineRangesBy r (negate $ sizeOfR r)

-- | Insert text at the beginning of all ranges in the buffer.
insertText :: Y.YiString -> BufAction ()
insertText txt = rangeDo_ $ \r@(Range s _) -> do
  insertAt s txt
  moveSameLineRangesBy r (Coord 0 (Y.length txt))

-- | Move all ranges to the location of the next occurence of the given text.
findNext :: Y.YiString -> BufAction ()
findNext pat = do
  res <- rangeDo $ \(Range _ e) -> do
    off <- findNextFrom pat e
    let end = moveCursorByN 1 off
    return $ Range off end
  setRanges res

-- | Get the 'Coord' of the next occurence of the given text after the given 'Coord'
findNextFrom :: Y.YiString -> Coord -> BufAction Coord
findNextFrom pat c = do
  txt <- getText
  let distance = txt ^. afterC c . asText . tillNext (Y.toText pat) . from asText . to sizeOf
  return (distance + c)

-- | Move all ranges to the location of the previous occurence of the given text.
findPrev :: Y.YiString -> BufAction ()
findPrev pat = do
  res <- rangeDo $ \(Range s _) -> do
    off <- findPrevFrom pat s
    let end = moveCursorByN 1 off
    return $ Range off end
  setRanges res

-- | Get the 'Coord' of the previous occurence of the given text before the given 'Coord'
findPrevFrom :: Y.YiString -> Coord -> BufAction Coord
findPrevFrom pat c = do
  txt <- getText
  let distance = txt ^. beforeC c . asText . tillPrev (Y.toText pat) . from asText . to sizeOf
  return (c - distance)

-- | Move all ranges by the given number of columns
moveRangesByN :: Int -> BufAction ()
moveRangesByN = overRanges . fmap . moveRangeByN

-- | Move all ranges by the given number of rows and columns
moveRangesByC :: Coord -> BufAction ()
moveRangesByC = overRanges . fmap . moveRange
