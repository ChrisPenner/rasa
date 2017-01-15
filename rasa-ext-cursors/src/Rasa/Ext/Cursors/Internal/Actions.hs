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
import qualified Data.Text as T

import Control.Lens
import Control.Lens.Text
import Rasa.Ext
import Rasa.Ext.Cursors.Internal.Base

-- | Moves all Ranges that are on the same END row as the given range by the coord's row and column
-- This is used to adjust cursors when things have been inserted/deleted before them in the row.
moveSameLineRangesBy :: Range -> Coord -> BufAction ()
moveSameLineRangesBy (Range _ (Coord endRow endCol)) amt = do
  let moveInLine r@(Range (Coord startRow startCol) _) = return $
        if endRow == startRow && startCol > endCol
           then moveRange amt r
           else r
  ranges <~ rangeDo moveInLine

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
  ranges .= res

-- | Get the 'Coord' of the next occurence of the given text after the given 'Coord'
findNextFrom :: Y.YiString -> Coord -> BufAction Coord
findNextFrom pat c = do
  distance <- use (text . afterC c . asText . tillNext (Y.toText pat) . from asText . to sizeOf)
  return (distance + c)

-- | Move all ranges to the location of the previous occurence of the given text.
findPrev :: Y.YiString -> BufAction ()
findPrev pat = do
  res <- rangeDo $ \(Range s _) -> do
    off <- findPrevFrom pat s
    let end = moveCursorByN 1 off
    return $ Range off end
  ranges .= res

-- | Get the 'Coord' of the previous occurence of the given text before the given 'Coord'
findPrevFrom :: Y.YiString -> Coord -> BufAction Coord
findPrevFrom pat c = do
  txt <- use text
  let Offset o = c^.from (asCoord txt)
  distance <- use (text . asText . before o . tillPrev (Y.toText pat) . to T.length .to negate)
  return ((Offset $ distance + o)^.asCoord txt)

-- | Move all ranges by the given number of columns
moveRangesByN :: Int -> BufAction ()
moveRangesByN n = overRanges $ return . moveRangeByN n

-- | Move all ranges by the given number of rows and columns
moveRangesByC :: Coord -> BufAction ()
moveRangesByC c = overRanges $ return . moveRange c
