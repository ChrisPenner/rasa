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
    Action
  , getBuffer
  , getEditor
  , exit

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
  , HasEditor
  , getText
  , getRange
  , getBufRef

  -- * Actions over Buffers
  , BufAction
  , liftAction
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

  -- * Working with Extensions
  -- | Extension states for ALL the extensions installed are stored in the same
  -- map keyed by their 'Data.Typeable.TypeRep' so if more than one extension
  -- uses the same type then they'll conflict. This is easily solved by simply
  -- using a newtype around any types which other extensions may use (your own
  -- custom types don't need to be wrapped). For example if your extension stores
  -- a counter as an Int, wrap it in your own custom Counter newtype when storing
  -- it.
  --
  -- Because Extension states are stored by their 'Data.Typeable.TypeRep', they must define an
  -- instance of 'Data.Typeable.Typeable', luckily GHC can derive this for you with
  -- @deriving Typeable@.
  --
  -- It is also required for all extension states to define an instance of
  -- 'Data.Default.Default', this is because accessing an extension which has not
  -- yet been stored will result in the default value.
  --
  -- If there's no default value that makes sense for your type, you can define
  -- a default of 'Data.Maybe.Nothing' and pattern match on its value when you
  -- access it.
  --
  -- Extensions may store state persistently for later access or for other
  -- extensions to access. Because Rasa can't possibly know the types of the
  -- state that extensions will store it uses a clever workaround wherein
  -- extension states are stored in a map of 'Data.Typeable.TypeRep' -> 'Rasa.Internal.Extensions.Ext'
  -- which is coerced into the proper type when it's extracted. The interface to
  -- extract or alter a given extension is to use the 'ext' and 'bufExt' lenses.
  -- Simply use them as though they were lenses to an object of your type and
  -- it'll all work out.
  --
  -- Since it's polymorphic, if ghc can't figure out the type the result is
  -- supposed to be then you'll need to help it out with a type annotation.
  -- In practice you won't typically need to do this unless you're
  -- doing something complicated.
  , HasExts(..)
  , ext
  , getExt
  , setExt
  , overExt

  , getBufExt
  , setBufExt
  , overBufExt

  -- * Events

  -- | 'dispatchEvent' and 'addListener' are key parts of working with extensions.
  -- Here's an example of how you might use them in some sort of clipboard extensions:
  --
  -- > -- The event type which is triggered whenever something is copied to clipboard
  -- > data Copied = Copied Y.YiString
  -- >
  -- > -- This registers functions to be run when something is copied.
  -- > -- you can see this is just addListener but with a more concrete type.
  -- > onCopy :: (Copied -> Action ()) -> Action ListenerId
  -- > onCopy = addListener
  -- >
  -- > -- This takes a Copied event and runs all the listeners associated.
  -- > -- You can see it's just 'dispatchEvent' with a more concrete type.
  -- > doCopy :: Copied -> Action ()
  -- > doCopy = dispatchEvent
  -- >
  -- > copier :: Action ()
  -- > copier = do
  -- > -- ... do some stuff
  -- > doCopy $ Copied copiedTxt
  --
  , dispatchEvent
  , addListener
  , addListener_
  , removeListener

  , dispatchBufEvent
  , addBufListener
  , addBufListener_
  , removeBufListener

  , ListenerId

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

  -- * Working with Async Events/Actions
  , dispatchActionAsync
  , dispatchEventAsync
  , asyncActionProvider
  , asyncEventProvider
  , Dispatcher

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
  , Renderable(..)
  , RenderInfo
  ) where

import Rasa.Internal.Action
import Rasa.Internal.Actions
import Rasa.Internal.BufAction
import Rasa.Internal.BufActions
import Rasa.Internal.Buffer
import Rasa.Internal.Editor
import Rasa.Internal.Events
import Rasa.Internal.Extensions
import Rasa.Internal.Listeners
import Rasa.Internal.Range
import Rasa.Internal.Styles
import Rasa.Internal.Text
import Rasa.Internal.Utility
