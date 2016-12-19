{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors
  ( moveCursorBy
  , moveCursorTo
  , moveCursorOffsetBy
  , moveCursorOffsetTo
  , cursors
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

import Control.Lens

import Control.Lens.Text
import Data.Typeable
import Data.Default

import qualified Data.Text as T
newtype Cursor = Cursor
  { _curs :: Coord
  } deriving (Show, Typeable)

makeLenses ''Cursor

instance Default Cursor where
  def = Cursor {
  _curs=Coord 0 0
}

coord :: Lens' Buffer Coord
coord = lens getter setter
  where getter buf = buf^.bufExt.curs
        setter buf new = let txt = buf^.rope
                          in buf & bufExt.curs .~ clampCoord txt new

offset :: Lens' Buffer Int
offset = lens getter setter
  where getter buf = let txt = buf^.rope
                      in buf^.coord.from (asCoord txt)
        setter buf new = let txt = buf^.rope
                          in buf & coord.from (asCoord txt) .~ new

displayCursor ::  BufAction ()
displayCursor = do
  o <- use offset
  addStyle $ Span o (o+1) (flair ReverseVideo)

cursors :: Scheduler ()
cursors = beforeRender $ bufDo displayCursor

moveCursorTo :: Coord -> BufAction ()
moveCursorTo c = coord .= c

moveCursorBy :: Coord -> BufAction ()
moveCursorBy c = coord %= addCoord c

moveCursorOffsetBy :: Int -> BufAction ()
moveCursorOffsetBy i = offset %= (+i)

moveCursorOffsetTo :: Int -> BufAction ()
moveCursorOffsetTo i = offset .= i

deleteChar :: BufAction ()
deleteChar = use offset >>= deleteCharAt

insertText :: T.Text -> BufAction ()
insertText txt = use offset >>= insertTextAt txt

findNext :: T.Text -> BufAction ()
findNext txt = do
  o <- use offset
  n <- use $ text.to (T.drop o).tillNext txt.to T.length
  moveCursorOffsetBy n

findPrev :: T.Text -> BufAction ()
findPrev txt = do
  c <- use offset
  n <- use $ text.before c.tillPrev txt.to T.length
  moveCursorOffsetBy (-n)
