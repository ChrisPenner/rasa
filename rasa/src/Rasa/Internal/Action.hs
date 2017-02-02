{-# language DeriveFunctor
  , MultiParamTypeClasses
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , RankNTypes
  , ExistentialQuantification
  , ScopedTypeVariables
  , TemplateHaskell
#-}
module Rasa.Internal.Action
  ( Action(..)
  , addListener
  , removeListener
  , dispatchEvent
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
  ) where

import Rasa.Internal.ActionMonads
import Rasa.Internal.BufAction
import Rasa.Internal.Editor
import Rasa.Internal.Buffer
import Rasa.Internal.Extensions

import Control.Lens
import Control.Monad.Free
import Control.Monad.State

import Data.Foldable
import Data.Default
import Data.Maybe
import Data.Typeable
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Unsafe.Coerce
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
addListener :: Typeable event => (event -> Action r) -> Action ListenerId
addListener listener = liftActionF $ AddListener listener id

-- | This removes a listener and prevents it from responding to any more events.
removeListener :: ListenerId -> Action ()
removeListener listenerId = liftActionF $ RemoveListener listenerId ()

-- | Adds a new async action provider
asyncActionProvider :: ((Action () -> IO ()) -> IO ()) -> Action ()
asyncActionProvider asyncActionProv = liftActionF $ AsyncActionProvider asyncActionProv ()

-- | Adds a new async event provider
asyncEventProvider :: forall event. Typeable event => ((event -> IO ()) -> IO ()) -> Action ()
asyncEventProvider asyncEventProv =
  liftActionF $ AsyncActionProvider (lmap toAction asyncEventProv) ()
    where toAction = lmap dispatchEvent

bufferDo :: [BufRef] -> BufAction r -> Action [r]
bufferDo bufRefs bufAct = liftActionF $ BufferDo bufRefs bufAct id

addBuffer :: Action BufRef
addBuffer = liftActionF $ AddBuffer id

-- | Returns an up-to-date list of all 'BufRef's
getBufRefs :: Action [BufRef]
getBufRefs = liftActionF $ GetBufRefs id

-- | Retrieve some extension state
getExt :: forall ext. (Typeable ext, Show ext, Default ext) => Action ext
getExt = liftActionF $ GetExt id

-- | Set some extension state
setExt :: forall ext. (Typeable ext, Show ext, Default ext) => ext -> Action ()
setExt newExt = liftActionF $ SetExt newExt ()

-- | Set some extension state
overExt :: forall ext. (Typeable ext, Show ext, Default ext) => (ext -> ext) -> Action ()
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
          mkListener :: forall event r. Typeable event => Int -> (event -> Action r) -> (Listener, ListenerId, TypeRep)
          mkListener n listenerFunc =
            let list = Listener listId (void . listenerFunc)
                listId = ListenerId n (typeRep (Proxy :: Proxy event))
                prox = typeRep (Proxy :: Proxy event)
             in (list, listId, prox)

    (RemoveListener listenerId@(ListenerId _ eventType) next) -> do
      listeners.at eventType._Just %= filter (notMatch listenerId)
      actionInterpreter next
        where
          notMatch idA (Listener idB _) = idA /= idB

    (DispatchEvent evt next) -> do
      listeners' <- use listeners
      let (Action action) = traverse_ ($ evt) (matchingListeners listeners')
      actionInterpreter (action >> next)

    (DispatchActionAsync asyncActionIO next) -> do
      asyncQueue <- use actionQueue
      let effect = (liftIO asyncActionIO >>= yield) >-> toOutput asyncQueue
      liftIO $ forkIO $ runEffect effect >> performGC
      actionInterpreter next

    (AsyncActionProvider dispatcherToIO next) -> do
      asyncQueue <- use actionQueue
      let dispatcher action =
            let effect = yield action >-> toOutput asyncQueue
             in void . forkIO $ runEffect effect >> performGC
      liftIO . forkIO $ dispatcherToIO dispatcher
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
matchingListeners :: forall a. Typeable a => Listeners -> [a -> Action ()]
matchingListeners listeners' = getListener <$> (listeners'^.at (typeRep (Proxy :: Proxy a))._Just)

getListener :: forall a. Listener -> (a -> Action ())
getListener = coerce
  where
    coerce :: Listener -> (a -> Action ())
    coerce (Listener _ x) = unsafeCoerce x
