{-# language DeriveFunctor
  , MultiParamTypeClasses
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , ExistentialQuantification
  , ScopedTypeVariables
  , TemplateHaskell
  , RankNTypes
#-}
module Rasa.Internal.Action
  ( Action(..)
  , addListener
  , removeListener
  , dispatchEvent
  , dispatchEvent_
  , dispatchEventAsync
  , dispatchActionAsync
  , asyncActionProvider
  , asyncEventProvider
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
  , runAction
  , evalAction
  , execAction
  , bootstrapAction
  , ActionState
  , mkActionState
  , Listener
  , ListenerId
  , Listeners
  , listeners
  , nextListenerId
  , actionQueue
  , Dispatcher
  ) where

import Rasa.Internal.ActionMonads
import Rasa.Internal.BufAction
import Rasa.Internal.Editor
import Rasa.Internal.Buffer
import Rasa.Internal.Extensions

import Control.Lens
import Control.Monad.Free
import Control.Monad.State

import Data.Default
import Data.Maybe
import Data.Typeable
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Pipes hiding (Proxy, next)
import Pipes.Concurrent hiding (Buffer)

-- | This contains all data representing the editor's state. It acts as the state object for an 'Action
data ActionState = ActionState
  { _ed :: Editor
  , _listeners :: Listeners
  , _nextListenerId :: Int
  , _nextBufId :: Int
  , _actionQueue :: Output (Action ())
  }
makeLenses ''ActionState

instance Show ActionState where
  show as = show (_ed as)

mkActionState :: Output (Action ()) -> ActionState
mkActionState out = ActionState
    { _ed=def
    , _listeners=def
    , _nextListenerId=0
    , _nextBufId=0
    , _actionQueue=out
    }

instance HasEditor ActionState where
  editor = ed

instance HasExts ActionState where
  exts = ed.exts

-- | This function takes an IO which results in some Event, it runs the IO
-- asynchronously and dispatches the event
dispatchEventAsync :: Typeable event => IO event -> Action ()
dispatchEventAsync ioEvent = liftActionF $ DispatchActionAsync (dispatchEvent <$> ioEvent) ()

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

-- | Adds a new listener
addListener :: (Typeable event, Typeable response, Monoid response) => (event -> Action response) -> Action ListenerId
addListener listener = liftActionF $ AddListener listener id

-- | This removes a listener and prevents it from responding to any more events.
removeListener :: ListenerId -> Action ()
removeListener listenerId = liftActionF $ RemoveListener listenerId ()

-- | Don't let the type signature confuse you; it's much simpler than it seems.
-- The first argument is a function which takes an action provider; the action provider
-- will be passed a dispatch function which can be called as often as you like with @Action ()@s.
-- When it is passed an 'Action' it forks off an IO to dispatch that 'Action' to the main event loop.
-- Note that the dispatch function calls forkIO on its own; so there's no need for you to do so.
--
-- Use this function when you have some long-running process which dispatches multiple 'Action's.
asyncActionProvider :: ((Action () -> IO ()) -> IO ()) -> Action ()
asyncActionProvider asyncActionProv = liftActionF $ AsyncActionProvider asyncActionProv ()

-- | This is a type alias to make defining your event provider functions easier;
-- It represents the function your event provider function will be passed to allow dispatching
-- events. Using this type requires the @RankNTypes@ language pragma.
type Dispatcher = forall event. Typeable event => event -> IO ()

-- | This allows long-running IO processes to provide Events to Rasa asyncronously.
--
-- Don't let the type signature confuse you; it's much simpler than it seems.
--
-- Let's break it down:
--
-- Using 'Dispatcher' with asyncEventProvider requires the @RankNTypes@ language pragma.
--
-- This type as a whole represents a function which accepts a 'Dispatcher' and returns an 'IO';
-- the dispatcher itself accepts data of ANY 'Typeable' type and emits it as an event (see "Rasa.Internal.Events").
--
-- When you call 'asyncEventProvider' you pass it a function which accepts a @dispatch@ function as an argument
-- and then calls it with various events within the resulting 'IO'.
--
-- Note that asyncEventProvider calls forkIO internally, so there's no need to do that yourself.
--
-- Here's an example which fires a @Timer@ event every second.
--
-- > {-# language RankNTypes #-}
-- > data Timer = Timer
-- > myTimer :: Dispatcher -> IO ()
-- > myTimer dispatch = forever $ dispatch Timer >> threadDelay 1000000
-- >
-- > myAction :: Action ()
-- > myAction = onInit $ asyncEventProvider myTimer
asyncEventProvider :: (Dispatcher -> IO ()) -> Action ()
asyncEventProvider asyncEventProv =
  liftActionF $ AsyncActionProvider (eventsToActions asyncEventProv) ()
    where
      eventsToActions :: (Dispatcher -> IO ()) -> (Action () -> IO ()) -> IO ()
      eventsToActions aEventProv dispatcher = aEventProv (dispatcher . dispatchEvent)

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

-- | Retrieve some extension state
getExt :: (Typeable ext, Show ext, Default ext) => Action ext
getExt = liftActionF $ GetExt id

-- | Set some extension state
setExt :: (Typeable ext, Show ext, Default ext) => ext -> Action ()
setExt newExt = liftActionF $ SetExt newExt ()

-- | Set some extension state
overExt :: (Typeable ext, Show ext, Default ext) => (ext -> ext) -> Action ()
overExt f = getExt >>= setExt . f

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

-- | Runs an Action into an IO
runAction :: ActionState -> Action a -> IO (a, ActionState)
runAction actionState (Action actionF) = flip runStateT actionState $ actionInterpreter actionF

-- | Evals an Action into an IO
evalAction :: ActionState -> Action a -> IO a
evalAction actionState action = fst <$> runAction actionState action

-- | Execs an Action into an IO
execAction :: ActionState -> Action a -> IO ActionState
execAction actionState action = snd <$> runAction actionState action

-- | Spawn the channels necessary to run action and do so.
bootstrapAction :: Action a -> IO a
bootstrapAction action = do
    (output, _) <- spawn unbounded
    evalAction (mkActionState output) action

-- | Interpret the Free Monad; in this case it interprets it down to an IO
actionInterpreter :: Free ActionF r -> StateT ActionState IO r
actionInterpreter (Free actionF) =
  case actionF of
    (LiftIO ioNext) ->
      liftIO ioNext >>= actionInterpreter

    (BufferDo bufRefs bufAct toNext) -> do
      results <- forM bufRefs $ \(BufRef bInd) ->
        use (buffers.at bInd) >>= traverse (handleBuf bInd)
      actionInterpreter . toNext $ catMaybes results
        where handleBuf bIndex buf = do
                let Action act = runBufAction bufAct buf
                (res, newBuffer) <- actionInterpreter act
                buffers.at bIndex ?= newBuffer
                return res

    (AddListener listenerF withListenerId) -> do
      n <- nextListenerId <<+= 1
      let (listener, listenerId, eventType) = mkListener n listenerF
      listeners %= M.insertWith mappend eventType [listener]
      actionInterpreter $ withListenerId listenerId
        where
          mkListener :: forall event response. (Typeable event, Typeable response, Monoid response) => Int -> (event -> Action response) -> (Listener, ListenerId, TypeRep)
          mkListener n listenerFunc =
            let list = Listener listId listenerFunc
                listId = ListenerId n (typeRep (Proxy :: Proxy event))
                prox = typeRep (Proxy :: Proxy event)
             in (list, listId, prox)

    (RemoveListener listenerId@(ListenerId _ eventType) next) -> do
      listeners.at eventType._Just %= filter (notMatch listenerId)
      actionInterpreter next
        where
          notMatch idA (Listener idB _) = idA /= idB

    (DispatchEvent evt toNext) -> do
      listeners' <- use listeners
      let (Action action) = mconcat <$> traverse ($ evt) (matchingListeners listeners')
      actionInterpreter (action >>= toNext)

    (DispatchActionAsync asyncActionIO next) -> do
      asyncQueue <- use actionQueue
      let effect = (liftIO asyncActionIO >>= yield) >-> toOutput asyncQueue
      liftIO . void . forkIO $ runEffect effect >> performGC
      actionInterpreter next

    (AsyncActionProvider dispatcherToIO next) -> do
      asyncQueue <- use actionQueue
      let dispatcher action =
            let effect = yield action >-> toOutput asyncQueue
             in void . forkIO $ runEffect effect >> performGC
      liftIO . void . forkIO $ dispatcherToIO dispatcher
      actionInterpreter next

    (AddBuffer toNext) -> do
      bufId <- nextBufId <+= 1
      buffers.at bufId ?= def
      actionInterpreter . toNext $ BufRef bufId

    (GetBufRefs toNext) ->
      use (buffers.to IM.keys) >>= actionInterpreter . toNext . fmap BufRef

    (GetExt toNext) ->
      use ext >>= actionInterpreter . toNext

    (SetExt new next) -> do
      ext .= new
      actionInterpreter next

    (GetEditor toNext) ->
      use ed >>= actionInterpreter . toNext

    (GetBuffer (BufRef bufInd) toNext) ->
      use (buffers.at bufInd) >>= actionInterpreter . toNext

    (Exit next) -> do
      exiting .= True
      actionInterpreter next

    (ShouldExit toNext) -> do
      ex <- use exiting
      actionInterpreter (toNext ex)

actionInterpreter (Pure res) = return res

-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners :: forall a r. (Typeable a, Typeable r) => Listeners -> [a -> Action r]
matchingListeners listeners' = catMaybes $ getListener <$> (listeners'^.at (typeRep (Proxy :: Proxy a))._Just)

-- | Extract the listener function from a listener
getListener :: forall a r. (Typeable a, Typeable r) => Listener -> Maybe (a -> Action r)
getListener (Listener _ x) = cast x
