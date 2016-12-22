module Rasa.Ext.Cursors
  ( cursorMain
  , deleteChar
  , insertText
  , findNext
  , findOffsetNext
  , findPrev
  , findOffsetPrev
  , eachCoord
  , coordsDo_
  , offsetsDo
  , offsetsDo_
  , addCursorCoordAt
  , addCursorOffsetAt
  , offsets
  , coords
  , eachOffset
  , coordsDo
  ) where

import Rasa.Ext.Cursors.Base
import Rasa.Ext.Cursors.Actions

import Rasa.Ext.Directive
import Rasa.Ext.Scheduler

cursorMain :: Scheduler ()
cursorMain = beforeRender $ bufDo displayCursor
