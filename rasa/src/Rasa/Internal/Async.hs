{-# language Rank2Types, ExistentialQuantification #-}
module Rasa.Internal.Async
  ( asyncEventProvider
  , Dispatcher
  , asyncActionProvider
  , dispatchEventAsync
  , dispatchActionAsync
) where

import Rasa.Internal.Action
import Rasa.Internal.Listeners

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

-- | This allows long-running IO processes to provide Events to Rasa asyncronously.
--
-- Don't let the type signature confuse you; it's much simpler than it seems.
--
-- Let's break it down:
--
-- @('Dispatcher' -> 'IO' ())@: 'Dispatcher' is a type alias just to make defining your own functions easier;
-- Using 'Dispatcher' with asyncEventProvider requires the @Rank2Types@ language pragma.
--
-- This type as a whole represents a function which accepts a 'Dispatcher' and returns an 'IO';
-- the dispatcher itself accepts data of ANY 'Typeable' type and emits it as an event (see the "Rasa.Internal.Events").
--
-- When you call 'asyncEventProvider' you pass it a function which accepts a @dispatch@ function as an argument
-- and then calls it with various events within the resulting 'IO'.
--
-- Note that asyncEventProvider calls forkIO internally, so there's no need to do that yourself.
--
-- Here's a simple example which fires a @Timer@ event every second.
--
-- > {-# language Rank2Types #-}
-- > data Timer = Timer
-- > myTimer :: Dispatcher -> IO ()
-- > myTimer dispatch = forever $ dispatch Timer >> threadDelay 1000000
-- >
-- > myAction :: Action ()
-- > myAction = onInit $ asyncEventProvider myTimer

asyncEventProvider :: (Dispatcher -> IO ()) -> Action ()
asyncEventProvider eventProvidingIO = do
  out <- use actionQueue
  liftIO $ void . forkIO $ eventProvidingIO (dispatchAction out . dispatchEvent)

-- | This is a type alias to make defining your event provider functions easier;
-- It represents the function your event provider function will be passed to allow dispatching
-- events. Using this type requires the @Rank2Types@ language pragma.
type Dispatcher = forall a. Typeable a => a -> IO ()

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
