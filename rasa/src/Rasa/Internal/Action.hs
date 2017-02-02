{-# language DeriveFunctor
  , MultiParamTypeClasses
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , RankNTypes
  , ExistentialQuantification
  , ScopedTypeVariables
  , TemplateHaskell #-}
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
import Rasa.Internal.Editor
import Rasa.Internal.Extensions

import Control.Arrow
import Control.Lens
import Control.Monad.Free
import Control.Monad.State

import Data.Foldable
import Data.Default
import Data.Typeable
import qualified Data.Map as M

import Unsafe.Coerce
import Pipes hiding (Proxy, next)
import Pipes.Concurrent hiding (Buffer)

-- | This contains all data representing the editor's state. It acts as the state object for an 'Action
data ActionState = ActionState
  { _ed :: Editor
  , _listeners :: Listeners
  , _nextListenerId :: Int
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
    , _actionQueue=out
    }

instance HasEditor ActionState where
  editor = ed

instance HasExts ActionState where
  exts = ed.exts

-- | Embeds a ActionF type into the Action Monad
liftActionF :: ActionF a -> Action a
liftActionF = Action . liftF

-- | Allows running IO in BufAction.
liftFIO :: IO r -> Action r
liftFIO = liftActionF . LiftIO

-- | Dispatches an Event
dispatchEvent :: Typeable event => event -> Action ()
dispatchEvent event = liftActionF $ DispatchEvent event ()

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
asyncEventProvider asyncEventProv = liftActionF $ AsyncActionProvider (lmap toAction asyncEventProv) ()
  where toAction = lmap dispatchEvent

bufferDo :: [BufRef] -> BufAction r -> Action [r]
bufferDo bufRefs bufAct = liftActionF $ BufferDo bufRefs bufAct id

addBuffer :: Action BufRef
addBuffer = liftActionF $ AddBuffer id

-- | Returns an up-to-date list of all 'BufRef's
getBufRefs :: Action [BufRef]
getBufRefs = liftActionF $ GetBufRefs id

instance MonadIO Action where
  liftIO = liftFIO

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

    (BufferDo bufRefs bufAct toNext) -> undefined

    (AddListener listenerF withListenerId) -> do
      n <- nextListenerId <<+= 1
      let mkListener :: forall event r. Typeable event => (event -> Action r) -> (Listener, ListenerId, TypeRep)
          mkListener listenerFunc = 
            let list = Listener listId (void . listenerFunc)
                listId = ListenerId n (typeRep (Proxy :: Proxy event))
                prox = typeRep (Proxy :: Proxy event)
             in (list, listId, prox)
          (listener, listenerId, eventType) = mkListener listenerF
      listeners %= M.insertWith mappend eventType [listener]
      actionInterpreter $ withListenerId listenerId

    (RemoveListener idA@(ListenerId _ eventType) next) -> do
      listeners.at eventType._Just %= filter listenerMatches
      actionInterpreter next
        where
          listenerMatches (Listener idB _) = idA /= idB

    (DispatchEvent evt next) -> do
      listeners' <- use listeners
      let (Action action) = traverse_ ($ evt) (matchingListeners listeners')
      actionInterpreter (action >> next)

    (DispatchActionAsync asyncActionIO next) -> do
      asyncQueue <- use actionQueue
      let effect = (liftIO asyncActionIO >>= yield) >-> toOutput asyncQueue
      liftIO $ void $ forkIO $ runEffect effect >> performGC
      actionInterpreter next

    (AsyncActionProvider dispatcherToIO next) -> do
      asyncQueue <- use actionQueue
      let dispatcher action = 
            let effect = yield action >-> toOutput asyncQueue
             in void . forkIO $ runEffect effect >> performGC
      liftIO $ void $ forkIO $ dispatcherToIO dispatcher
      actionInterpreter next

  -- buffers %= insert n (mkBuffer txt)
  -- let bufRef = BufRef n
  -- dispatchEvent (BufAdded bufRef)
  -- return bufRef

-- getBufRefs = fmap BufRef <$> use (buffers.to keys)

actionInterpreter (Pure res) = return res


-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners :: forall a. Typeable a => Listeners -> [a -> Action ()]
matchingListeners listeners' = getListener <$> (listeners'^.at (typeRep (Proxy :: Proxy a))._Just)

getListener :: forall a. Listener -> (a -> Action ())
getListener = coerce
  where
    coerce :: Listener -> (a -> Action ())
    coerce (Listener _ x) = unsafeCoerce x
