{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors
  ( moveCursorBy
  , moveCursorTo
  , moveCursorCoord
  , cursors
  , deleteChar
  , insertText
  , findNext
  , findPrev
  ) where

import Rasa.Ext
import Rasa.Ext.Scheduler
import Rasa.Ext.Directive
import Rasa.Ext.Style

import Control.Lens

import Control.Lens.Text as TL
import Data.Typeable
import Data.Default

import qualified Data.Text as T
newtype Cursor = Cursor
  { _curs :: Int
  } deriving (Show, Typeable)

makeLenses ''Cursor

instance Default Cursor where
  def = Cursor {
  _curs=0
}

cursor :: Lens' Buffer Int
cursor = bufExt.curs

displayCursor ::  BufAction ()
displayCursor = do
  c <- use cursor
  txt <- use text
  addStyle (Span (c^.asCoord txt) ((c+1)^.asCoord txt) (flair ReverseVideo))

cursors :: Scheduler ()
cursors = beforeRender $ bufDo displayCursor

moveCursorTo :: Int -> BufAction ()
moveCursorTo n = do
  mx <- use (text.to T.length)
  cursor .= clamp 0 mx n

moveCursorBy :: Int -> BufAction ()
moveCursorBy n = do
  mx <- use $ text.to T.length
  cursor %= clamp 0 mx . (+n)

moveCursorCoord :: Coord -> BufAction ()
moveCursorCoord coord = do
  txt <- use text
  cursor.asCoord txt %= addCoord coord

deleteChar :: BufAction ()
deleteChar = do
  c <- use cursor
  deleteCharAt c

insertText :: T.Text -> BufAction ()
insertText txt = do
  c <- use cursor
  insertTextAt c txt

findNext :: T.Text -> BufAction ()
findNext txt = do
  c <- use cursor
  n <- use $ text.after c.tillNext txt.to T.length
  moveCursorBy n

findPrev :: T.Text -> BufAction ()
findPrev txt = do
  c <- use cursor
  n <- use $ text.before c.tillPrev txt.to T.length
  moveCursorBy (-n)

