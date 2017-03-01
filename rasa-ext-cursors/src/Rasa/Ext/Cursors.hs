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
  , addRange
  , getRanges
  , setRanges
  , rangeDo
  , rangeDo_
  , overRanges
  , moveRangesByN
  , moveRangesByC
  ) where

import Rasa.Ext
import Rasa.Ext.Cursors.Internal.Base
import Rasa.Ext.Cursors.Internal.Actions

-- | Registers listeners for the extension. The user should add this to their config.
cursors :: App ()
cursors = onInit . onBufAdded $
  \(BufAdded bufRef) -> bufDo_ bufRef setStyleProvider
