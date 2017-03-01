----------------------------------------------------------------------------
-- |
-- Module      :  Rasa.Ext
-- Copyright   :  (C) 2016 Chris Penner
-- License     :  MIT
-- Maintainer  :  Chris Penner <christopher.penner@gmail.com>
--
-- This module contains the public API for building an extension for Rasa. It
-- re-exports the parts of rasa that are public API for creating extensions.
--
-- There are two main things that an extension can do, either react
-- to editor events, or expose useful actions and/or state for other extensions
-- to use.
--
-- Whether performing its own actions or being used by a different extension
-- an extension will want to define some 'Action's to perform. Actions
-- can operate over buffers or even perform IO and comprise the main way in which
-- extensons do what they need to do. Read more here: 'Action', 'BufAction'.
--
-- To sum it all up, Here's an example of a simple logging extension that
-- simply writes each keypress to a file.
--
-- > logKeypress :: Keypress -> Action ()
-- > logKeypress (Keypress char _) = liftIO $ appendFile "logs" ("You pressed " ++ [char] ++ "\n")
-- > logKeypress _ = return ()
-- >
-- > logger :: Action ()
-- > logger = do
-- >   onInit $ liftIO $ writeFile "logs" "==Logs==\n"
-- >   -- Listeners should also be registered using 'onInit'.
-- >   -- It ensures all listeners are ready before any actions occur.
-- >   onInit $ onKeypress logKeypress
-- >   onExit $ liftIO $ appendFile "logs" "==Done=="
--
-- Check out this tutorial on building extensions, it's also just a great way to learn
-- how the editor works: <https://github.com/ChrisPenner/rasa/blob/master/docs/Building-An-Extension.md Extension-Guide>.
----------------------------------------------------------------------------

module Rasa.Ext
  (
  -- * Editor Actions
  getBuffer

  -- * Managing Buffers
  , addBuffer
  , nextBufRef
  , prevBufRef
  , getBufRefs

  -- * Working with Buffers
  , Buffer
  , HasBuffer(..)
  , BufRef
  , text
  , getText
  , getRange
  , getBufRef

  -- * Actions over Buffers
  , BufAction
  , bufDo
  , bufDo_
  , buffersDo
  , buffersDo_

  -- * Working with Text
  , overRange
  , replaceRange
  , deleteRange
  , insertAt
  , sizeOf
  , getLineRange

  , getBufExt
  , setBufExt
  , overBufExt

  -- * Events

  , dispatchBufEvent
  , addBufListener
  , addBufListener_
  , removeBufListener

  -- * Built-in Events
  , Keypress(..)
  , Mod(..)
  , dispatchKeypress
  , BufAdded(..)
  , BufTextChanged(..)

  -- * Built-in Event Listeners
  , onInit
  , afterInit
  , beforeEveryEvent
  , beforeEveryEvent_
  , beforeEveryRender
  , beforeEveryRender_
  , onEveryRender
  , onEveryRender_
  , afterEveryRender
  , afterEveryRender_
  , onExit
  , onBufAdded
  , onBufAdded_
  , onEveryNewBuffer
  , onEveryNewBuffer_
  , onBufTextChanged
  , onKeypress

   -- * Ranges
  , Range(..)
  , CrdRange
  , Coord
  , Coord'(..)
  , Offset(..)
  , Span(..)
  , overRow
  , overCol
  , coordRow
  , coordCol
  , overBoth
  , combineSpans
  , asCoord
  , clampCoord
  , clampRange
  , rStart
  , rEnd
  , sizeOfR
  , afterC
  , beforeC
  , moveRange
  , moveRangeByN
  , moveCursorByN

  -- * Styles
  -- | A common representation for text styling
  , fg
  , bg
  , flair
  , Color(..)
  , Flair(..)
  , Style(..)
  , Styles
  , addStyleProvider
  , getStyles
  , styleText

   -- * Useful Utilities
  , asText
  , asString
  , asLines
  , clamp
  , cropToViewport

  -- * Common Types/Interfaces
  -- | These exist to help unify the interfaces of many different extensions without
  -- requiring them to depend upon each other. Use them liberally in your own extensions.
  , Width
  , Height
  , ScrollPos
  , RenderInfo(..)
  , Renderable(..)

  , module Reflex
  ) where

import Reflex
import Rasa.Internal.BufActions
import Rasa.Internal.Buffer
import Rasa.Internal.Events
import Rasa.Internal.Listeners
import Rasa.Internal.Range
import Rasa.Internal.Styles
import Rasa.Internal.Text
import Rasa.Internal.Utility
