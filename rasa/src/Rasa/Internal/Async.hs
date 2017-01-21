module Rasa.Internal.Async
  ( asyncEventProvider
  , asyncActionProvider
  , dispatchEventAsync
  , dispatchActionAsync
) where

import Rasa.Internal.Action
import Rasa.Internal.Scheduler

import Control.Lens
import Control.Monad.IO.Class

import Data.Typeable

import Pipes
import Pipes.Concurrent

-- | A helper which when given an output channel results in a function from @Action () -> IO ()@ which
-- dispatches any actions it's called with asyncronously to the main event loop.
dispatchAction :: Output (Action ()) -> Action () -> IO ()
dispatchAction output act =
   void . forkIO $ do runEffect $ yield act >-> toOutput output
                      performGC

-- | This function takes an IO which results in some Event, it runs the IO
-- asynchronously and dispatches the event
dispatchEventAsync :: Typeable a => IO a -> Action ()
dispatchEventAsync getEventIO = do
  out <- use actionQueue
  liftIO $ void . forkIO $ do runEffect $ producer >-> toOutput out
                              performGC
  where
    producer :: Producer (Action ()) IO ()
    producer = do
          evt <- lift getEventIO
          yield (dispatchEvent evt)

-- | Don't let the type signature confuse you; it's much simpler than it seems.
-- The first argument is a function which takes an event provider; the event provider
-- will be passed a dispatch function which can be called as often as you like with Events of any 'Typeable' type.
-- (see the "Rasa.Internal.Events")
-- When it is passed an 'Action' it forks off an IO to dispatch that 'Action' to the main event loop.
-- Note that the dispatch function calls forkIO on its own; so there's no need for you to do so.
--
-- Use this function when you have some long-running process which dispatches multiple 'Action's.

asyncEventProvider :: Typeable a => ((a -> IO ()) -> IO ()) -> Action ()
asyncEventProvider eventProvidingIO = do
  out <- use actionQueue
  liftIO $ void . forkIO $ eventProvidingIO (dispatchAction out . dispatchEvent)

-- | Don't let the type signature confuse you; it's much simpler than it seems.
-- The first argument is a function which takes an action provider; the action provider
-- will be passed a dispatch function which can be called as often as you like with @Action ()@s.
-- When it is passed an 'Action' it forks off an IO to dispatch that 'Action' to the main event loop.
-- Note that the dispatch function calls forkIO on its own; so there's no need for you to do so.
--
-- Use this function when you have some long-running process which dispatches multiple 'Action's.

asyncActionProvider :: ((Action () -> IO ()) -> IO ()) -> Action ()
asyncActionProvider actionProvidingIO = do
  out <- use actionQueue
  liftIO $ void . forkIO $ actionProvidingIO (dispatchAction out)

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
-- >   txt <- focusDo $ use text
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
-- >   newTxt <- focusDo (use text)
-- >   if newTxt == oldTxt
-- >     -- If the text is the same as it was, we can apply the transformation
-- >     then focusDo (text .= capitalized)
-- >     -- Otherwise we can choose to re-queue the whole action and try again
-- >     -- Or we could just give up.
-- >     else asyncCapitalize

dispatchActionAsync ::  IO (Action ()) -> Action ()
dispatchActionAsync asyncIO = do
  queue <- use actionQueue
  liftIO $ void $ forkIO $ do runEffect $ producer >-> toOutput queue
                              performGC
  where producer = do
          action <- lift asyncIO
          yield action
