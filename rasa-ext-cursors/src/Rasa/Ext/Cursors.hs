module Rasa.Ext.Cursors
  ( cursorMain
  , delete
  , insertText
  , findNext
  , findNextFrom
  , findPrev
  , findPrevFrom
  , eachRange
  , moveRanges
  , ranges
  ) where

import Rasa.Ext.Cursors.Base
import Rasa.Ext.Cursors.Actions

import Rasa.Ext.Directive
import Rasa.Ext.Scheduler

cursorMain :: Scheduler ()
cursorMain = beforeRender $ bufDo displayRange
