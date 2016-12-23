module Rasa.Ext.Cursors.Actions
  ( delete
  , insertText
  , findNext
  , findPrev
  , findNextFrom
  , findPrevFrom
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
  let endCrd = cursorToCoord txt end
  res <- rangeDo $ \r@(Range s _) -> do
    let startCrd = cursorToCoord txt s
    if crdGT endCrd startCrd
       then return $ moveRange txt amt r
       else return r
  ranges .= res
    where crdGT (Coord row col) (Coord row' col') =
            row == row' && col' > col

-- deleteChar :: BufAction ()
-- deleteChar = deleteN 1

delete :: BufAction ()
delete = rangeDo_ $ \r -> do
  deleteRange r
  amt <- rangeSize r
  moveSameLineRangesBy r (Left . Offset $ amt)

insertText :: T.Text -> BufAction ()
insertText txt = rangeDo_ $ \r@(Range s _) -> do
  insertAt s txt
  moveSameLineRangesBy r (Left . Offset . T.length $ txt)

findNext :: T.Text -> BufAction ()
findNext pat = do
  txt <- use rope
  res <- rangeDo $ \(Range _ e) -> do
    off <- findNextFrom pat e
    return $ Range off (moveCursor txt (Left . Offset $ 1) off)
  ranges .= res

findNextFrom :: T.Text -> Cursor -> BufAction Cursor
findNextFrom pat c = do
  txt <- use rope
  let Offset o = cursorToOffset txt c
  distance <- use (text . after o . tillNext pat . to T.length)
  return (Left . Offset $ distance + o)

findPrev :: T.Text -> BufAction ()
findPrev pat = do
  txt <- use rope
  res <- rangeDo $ \(Range _ e) -> do
    off <- findPrevFrom pat e
    return $ Range off (moveCursor txt (Left . Offset $ 1) off)
  ranges .= res

findPrevFrom :: T.Text -> Cursor -> BufAction Cursor
findPrevFrom pat c = do
  txt <- use rope
  let Offset o = cursorToOffset txt c
  distance <- use (text . before o . tillPrev pat . to T.length .to negate)
  return (Left . Offset $ distance + o)
