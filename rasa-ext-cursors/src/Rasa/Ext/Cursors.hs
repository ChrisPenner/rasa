module Rasa.Ext.Cursors
  ( moveCoordsBy
  , moveCoordsBy'
  , moveCoordsTo
  , moveCoordsTo'
  , moveOffsetsBy
  , moveOffsetsBy'
  , moveOffsetsTo
  , moveOffsetsTo'
  , cursorMain
  , deleteChar
  , insertText
  , findNext
  , findNextAt
  , findPrev
  , findPrevAt
  , coordsDo
  , coordsDo_
  , offsetsDo
  , offsetsDo_
  , addCursorCoordAt
  , addCursorOffsetAt
  , offsets
  , coords
  , addCoord
  , Coord(..)
  ) where

import Rasa.Ext.Cursors.Base
import Rasa.Ext.Cursors.Types
import Rasa.Ext.Cursors.Actions

import Rasa.Ext
import Rasa.Ext.Directive
import Rasa.Ext.Scheduler

cursorMain :: Scheduler ()
cursorMain = beforeRender $ bufDo displayCursor
