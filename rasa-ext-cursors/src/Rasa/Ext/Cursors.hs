module Rasa.Ext.Cursors
  (
  -- * Main
  cursors
  -- * Actions
  , delete
  , insertText
  , findNext
  , findNextFrom
  , findPrev
  , findPrevFrom

  -- * Working with Cursor Ranges
  , eachRange
  , addRange
  , ranges
  , rangeDo
  , rangeDo_
  , overRanges
  , moveRangesByN
  , moveRangesByC
  ) where

import Rasa.Ext.Cursors.Base
import Rasa.Ext.Cursors.Actions

import Rasa.Ext

-- | Registers hooks for the extension. The user should add this to their config.
cursors :: Scheduler ()
cursors = beforeRender $ bufDo displayRange
