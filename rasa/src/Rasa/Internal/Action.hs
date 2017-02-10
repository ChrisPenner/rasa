module Rasa.Internal.Action
  ( Action(..)
  , dispatchActionAsync
  , asyncActionProvider
  , bufferDo
  , addBuffer
  , getBufRefs
  , getExt
  , setExt
  , overExt
  , exit
  , shouldExit
  , getBuffer
  , getEditor
  ) where

import Rasa.Internal.ActionMonads
import Rasa.Internal.Editor
import Rasa.Internal.Buffer
import Rasa.Internal.Extensions

-- | dispatchActionAsync allows you to perform a task asynchronously and then apply the
-- result. In @dispatchActionAsync asyncAction@, @asyncAction@ is an IO which resolves to
-- an Action, note that the context in which the second action is executed is
-- NOT the same context in which dispatchActionAsync is called; it is likely that text and
-- other state have changed while the IO executed, so it's a good idea to check
-- (inside the applying Action) that things are in a good state before making
-- changes. Here's an example:
--
-- > asyncCapitalize :: Action ()
-- > asyncCapitalize = do
-- >   txt <- focusDo getText
-- >   -- We give dispatchActionAsync an IO which resolves in an action
-- >   dispatchActionAsync $ ioPart txt
-- >
-- > ioPart :: Text -> IO (Action ())
-- > ioPart txt = do
-- >   result <- longAsyncronousCapitalizationProgram txt
-- >   -- Note that this returns an Action, but it's still wrapped in IO
-- >   return $ maybeApplyResult txt result
-- >
-- > maybeApplyResult :: Text -> Text -> Action ()
-- > maybeApplyResult oldTxt capitalized = do
-- >   -- We get the current buffer's text, which may have changed since we started
-- >   newTxt <- focusDo getText
-- >   if newTxt == oldTxt
-- >     -- If the text is the same as it was, we can apply the transformation
-- >     then focusDo (setText capitalized)
-- >     -- Otherwise we can choose to re-queue the whole action and try again
-- >     -- Or we could just give up.
-- >     else asyncCapitalize
dispatchActionAsync :: IO (Action ()) -> Action ()
dispatchActionAsync ioAction = liftActionF $ DispatchActionAsync ioAction ()

-- | Don't let the type signature confuse you; it's much simpler than it seems.
-- The first argument is a function which takes an action provider; the action provider
-- will be passed a dispatch function which can be called as often as you like with @Action ()@s.
-- When it is passed an 'Action' it forks off an IO to dispatch that 'Action' to the main event loop.
-- Note that the dispatch function calls forkIO on its own; so there's no need for you to do so.
--
-- Use this function when you have some long-running process which dispatches multiple 'Action's.
--
-- Here's an example which fires a @Timer@ event every second.
--
-- > data Timer = TimerFired
-- > dispatchTimer :: Action ()
-- > dispatchTimer = mkDispatcher Timer
-- > myTimer :: (Action () -> IO ()) -> IO ()
-- > myTimer dispatch = forever $ dispatch dispatchTimer >> threadDelay 1000000
-- >
-- > myAction :: Action ()
-- > myAction = onInit $ asyncActionProvider myTimer

asyncActionProvider :: ((Action () -> IO ()) -> IO ()) -> Action ()
asyncActionProvider asyncActionProv = liftActionF $ AsyncActionProvider asyncActionProv ()

-- | Runs a BufAction over the given BufRefs, returning any results.
--
-- Result list is not guaranteed to be the same length or positioning as input BufRef list; some buffers may no
-- longer exist.
bufferDo :: [BufRef] -> BufAction r -> Action [r]
bufferDo bufRefs bufAct = liftActionF $ BufferDo bufRefs bufAct id

-- | Adds a new buffer and returns the BufRef
addBuffer :: Action BufRef
addBuffer = liftActionF $ AddBuffer id

-- | Returns an up-to-date list of all 'BufRef's
getBufRefs :: Action [BufRef]
getBufRefs = liftActionF $ GetBufRefs id

-- | Retrieve the entire editor state. This is read-only for logging/rendering/debugging purposes only.
getEditor :: Action Editor
getEditor = liftActionF $ GetEditor id

-- | Retrieve a buffer. This is read-only for logging/rendering/debugging purposes only.
getBuffer :: BufRef -> Action (Maybe Buffer)
getBuffer bufRef = liftActionF $ GetBuffer bufRef id

-- | This signals to the editor that you'd like to shutdown. The current events
-- will finish processing, then the 'Rasa.Internal.Listeners.onExit' event will be dispatched,
-- then the editor will exit.
exit :: Action ()
exit = liftActionF $ Exit ()

shouldExit :: Action Bool
shouldExit = liftActionF $ ShouldExit id
