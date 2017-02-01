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
  , runAction
  , evalAction
  , execAction
  , bootstrapAction
  , ActionState
  , mkActionState
  , Listener(..)
  , ListenerId(..)
  , Listeners
  , listeners
  , nextListenerId
  , actionQueue
  ) where

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

-- | A wrapper around event listeners so they can be stored in 'Listeners'.
data Listener = forall a. Listener ListenerId (a -> Action ())

-- | An opaque reverence to a specific registered event-listener.
-- A ListenerId is used only to remove listeners later with 'Rasa.Internal.Listeners.removeListener'.
data ListenerId =
  ListenerId Int TypeRep

instance Eq ListenerId where
  ListenerId a _ == ListenerId b _ = a == b

-- | A map of Event types to a list of listeners for that event
type Listeners = M.Map TypeRep [Listener]

-- | Free Monad Actions for Action
data ActionF state next =
  LiftState (state -> (next, state))
  | LiftIO (IO next)
  | forall event r. Typeable event => AddListener (event -> Action r) (ListenerId -> next)
  | RemoveListener ListenerId next
  | forall event. Typeable event => DispatchEvent event next
  | DispatchActionAsync (IO (Action ())) next
  | AsyncActionProvider ((Action () -> IO ()) -> IO ()) next

instance Functor (ActionF s) where
  fmap f (LiftState stateF) = LiftState (first f <$> stateF)
  fmap f (LiftIO ioNext) = LiftIO $ f <$> ioNext
  fmap f (AddListener listener next) = AddListener listener $ f <$> next
  fmap f (RemoveListener listenerId next) = RemoveListener listenerId $ f next
  fmap f (DispatchEvent event next) = DispatchEvent event (f next)
  fmap f (DispatchActionAsync ioAction next) = DispatchActionAsync ioAction (f next)
  fmap f (AsyncActionProvider actionProvider next) = AsyncActionProvider actionProvider (f next)

-- | This is a monad for performing actions against the editor.
-- You can register Actions to be run in response to events using 'Rasa.Internal.Listeners.onEveryTrigger'
--
-- Within an Action you can:
--
--      * Use liftIO for IO
--      * Access/edit extensions that are stored globally, see 'ext'
--      * Embed any 'Action's exported other extensions
--      * Embed buffer actions using 'Rasa.Internal.Actions.bufDo' or 'Rasa.Internal.Actions.buffersDo'
--      * Add\/Edit\/Focus buffers and a few other Editor-level things, see the "Rasa.Internal.Actions" module.
newtype Action a = Action
  { getAction :: Free (ActionF Editor) a
  } deriving (Functor, Applicative, Monad)

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
liftActionF :: ActionF Editor a -> Action a
liftActionF = Action . liftF

-- | Allows running state actions over ActionState; used to lift mtl state functions
liftState :: (Editor -> (a, Editor)) -> Action a
liftState = liftActionF . LiftState

-- | Allows running IO in BufAction.
liftFIO :: IO r -> Action r
liftFIO = liftActionF . LiftIO

-- | Dispatches an Event
dispatchEvent :: Typeable event => event -> Action ()
dispatchEvent event = liftActionF $ DispatchEvent event ()

-- | Dispatches an Event asyncronously
dispatchEventAsync :: Typeable event => IO event -> Action ()
dispatchEventAsync ioEvent = liftActionF $ DispatchActionAsync (dispatchEvent <$> ioEvent) ()

-- | Dispatches an Action asyncronously
dispatchActionAsync :: IO (Action ()) -> Action ()
dispatchActionAsync ioAction = liftActionF $ DispatchActionAsync ioAction ()

-- | Adds a new listener
addListener :: Typeable event => (event -> Action r) -> Action ListenerId
addListener listener = liftActionF $ AddListener listener id

-- | Adds a new listener
removeListener :: ListenerId -> Action ()
removeListener listenerId = liftActionF $ RemoveListener listenerId ()

-- | Adds a new async action provider
asyncActionProvider :: ((Action () -> IO ()) -> IO ()) -> Action ()
asyncActionProvider asyncActionProv = liftActionF $ AsyncActionProvider asyncActionProv ()

-- | Adds a new async event provider
asyncEventProvider :: forall event. Typeable event => ((event -> IO ()) -> IO ()) -> Action ()
asyncEventProvider asyncEventProv = liftActionF $ AsyncActionProvider (lmap toAction asyncEventProv) ()
  where toAction = lmap dispatchEvent

instance (MonadState Editor) Action where
  state = liftState

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
actionInterpreter :: Free (ActionF Editor) r -> StateT ActionState IO r
actionInterpreter (Free actionF) =
  case actionF of
    (LiftState stateFunc) ->
      zoom ed (state stateFunc) >>= actionInterpreter

    (LiftIO ioNext) ->
      liftIO ioNext >>= actionInterpreter

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

  -- | AsyncActionProvider ((Action () -> IO ()) -> IO ()) next
    (AsyncActionProvider dispatcherToIO next) -> do
      asyncQueue <- use actionQueue
      let dispatcher action = 
            let effect = yield action >-> toOutput asyncQueue
             in void . forkIO $ runEffect effect >> performGC
      liftIO $ dispatcherToIO dispatcher
      actionInterpreter next


actionInterpreter (Pure res) = return res


-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners :: forall a. Typeable a => Listeners -> [a -> Action ()]
matchingListeners listeners' = getListener <$> (listeners'^.at (typeRep (Proxy :: Proxy a))._Just)

getListener :: forall a. Listener -> (a -> Action ())
getListener = coerce
  where
    coerce :: Listener -> (a -> Action ())
    coerce (Listener _ x) = unsafeCoerce x

-- makeListener :: forall a b. Typeable a => (a -> Action b) -> Action (ListenerId, Listener)
-- makeListener listenerFunc = do
--   n <- nextListenerId <<+= 1
--   let listenerId = ListenerId n (typeRep (Proxy :: Proxy a))
--       listenerFunc' = void . listenerFunc
--   return (listenerId, Listener listenerId listenerFunc')

-- | This registers an event listener, as long as the listener is well-typed similar to this:
--
-- @MyEventType -> Action ()@ then it will be triggered on all dispatched events of type @MyEventType@.
-- It returns an ID which may be used with 'removeListener' to cancel the listener
-- onEveryTrigger :: forall a b. Typeable a => (a -> Action b) -> Action ListenerId
-- onEveryTrigger listenerFunc = do
--   (listenerId, listener) <- makeListener listenerFunc
--   listeners %= insertWith mappend (typeRep (Proxy :: Proxy a)) [listener]
--   return listenerId


-- | This removes a listener and prevents it from responding to any more events.
-- removeListener :: ListenerId -> Action ()
-- removeListener hkIdA@(ListenerId _ typ) =
--   listeners.at typ._Just %= filter listenerMatches
--     where
--       listenerMatches (Listener hkIdB _) = hkIdA /= hkIdB


-- | This function takes an IO which results in some Event, it runs the IO
-- asynchronously and dispatches the event
-- dispatchEventAsync :: Typeable a => IO a -> Action ()
-- dispatchEventAsync getEventIO = do
--   out <- use actionQueue
--   liftIO $ void . forkIO $ do runEffect $ producer >-> toOutput out
--                               performGC
--   where
--     producer :: Producer (Action ()) IO ()
--     producer = do
--           evt <- lift getEventIO
--           yield (dispatchEvent evt)


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

-- dispatchActionAsync ::  IO (Action ()) -> Action ()
-- dispatchActionAsync asyncIO = do
--   queue <- use actionQueue
--   liftIO $ void $ forkIO $ do runEffect $ producer >-> toOutput queue
--                               performGC
--   where producer = do
--           action <- lift asyncIO
--           yield action
