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

import Rasa.Ext
import Rasa.Ext.Cursors.Internal.Base
import Rasa.Ext.Cursors.Internal.Actions

import Control.Monad

-- | Registers hooks for the extension. The user should add this to their config.
cursors :: Action ()
cursors = void . beforeRender $ buffersDo_ displayRange
