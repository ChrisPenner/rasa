module Rasa.Ext.Cursors
  ( cursorMain
  , delete
  , insertText
  , findNext
  , findNextFrom
  , findPrev
  , findPrevFrom
  , eachRange
  , ranges
  , rangeDo
  , rangeDo_
  , overRanges
  , moveRangesByN
  , moveRangesByC
  , addRange
  ) where

import Rasa.Ext.Cursors.Base
import Rasa.Ext.Cursors.Actions

import Rasa.Ext.Directive
import Rasa.Ext.Scheduler

cursorMain :: Scheduler ()
cursorMain = beforeRender $ bufDo displayRange
