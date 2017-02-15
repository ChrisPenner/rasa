{-# language
  GADTs
  , ExistentialQuantification
  , ScopedTypeVariables
  , RankNTypes
#-}

module Rasa.Internal.Listeners
  ( dispatchEvent
  , addListener
  , addListener_
  , removeListener

  , dispatchBufEvent
  , addBufListener
  , addBufListener_
  , removeBufListener

  , Dispatcher
  , ListenerId

  , onInit
  , dispatchInit

  , afterInit
  , dispatchAfterInit

  , beforeEveryRender
  , beforeEveryRender_
  , dispatchBeforeRender

  , beforeEveryEvent
  , beforeEveryEvent_
  , dispatchBeforeEvent

  , onEveryRender
  , onEveryRender_
  , dispatchOnRender

  , afterEveryRender
  , afterEveryRender_
  , dispatchAfterRender

  , onExit
  , dispatchExit

  , onBufAdded
  , dispatchBufAdded

  , onBufTextChanged
  , dispatchBufTextChanged

  , onKeypress
  , dispatchKeypress

  , asyncEventProvider
  , dispatchEventAsync
  ) where

import Rasa.Internal.Action
import Rasa.Internal.BufAction
import Rasa.Internal.Events
import Rasa.Internal.Extensions

import Control.Lens
import Control.Monad
import Data.Default
import Data.Typeable
import Data.Maybe
import qualified Data.Map as M

-- | A wrapper around event listeners so they can be stored in 'Listeners'.
data Listener where
  Listener :: (Typeable eventType, Typeable result, Monoid result, HasExtMonad m) => TypeRep -> ListenerId -> (eventType -> m result) -> Listener

instance Show Listener where
  show (Listener rep (ListenerId n _) _) = "<Listener #" ++ show n ++ ", " ++ show rep ++ ">"

-- | An opaque reverence to a specific registered event-listener.
-- A ListenerId is used only to remove listeners later with 'Rasa.Internal.Listeners.removeListener'.
data ListenerId =
  ListenerId Int TypeRep
  deriving Show

instance Eq ListenerId where
  ListenerId a _ == ListenerId b _ = a == b

-- | A map of Event types to a list of listeners for that event
type Listeners = M.Map TypeRep [Listener]

-- | Store the listeners in extensions
data LocalListeners =
  LocalListeners Int Listeners
  deriving Show

instance Default LocalListeners where
  def = LocalListeners 0 M.empty

-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners :: forall eventType result m. (Typeable eventType, Typeable result, HasExtMonad m) => Listeners -> [eventType -> m result]
matchingListeners listeners = catMaybes $ (getListener :: Listener -> Maybe (eventType -> m result)) <$> (listeners^.at (typeRep (Proxy :: Proxy eventType))._Just)

-- | Extract the listener function from eventType listener
getListener :: Typeable expected => Listener -> Maybe expected
getListener (Listener _ _ x) = cast x


-- | Trigger an 'Action' on a 'Keypress'
onKeypress :: (Keypress -> Action result) -> Action ListenerId
onKeypress actionF = addListener (void <$> actionF)

-- | Dispatch a 'Keypress' event.
dispatchKeypress :: Keypress -> Action ()
dispatchKeypress = dispatchEvent

-- | Registers an action to be performed during the Initialization phase.
--
-- This phase occurs exactly ONCE when the editor starts up.
-- Though arbitrary actions may be performed in the configuration block;
-- it's recommended to embed such actions in the onInit event listener
-- so that all event listeners are registered before anything Actions occur.
onInit :: Action result -> Action ()
onInit action = void $ addListener (const (void action) :: Init -> Action ())

-- | Dispatch the 'Init' action.
dispatchInit :: Action ()
dispatchInit = dispatchEvent Init

afterInit :: Action a -> Action ()
afterInit action = void $ addListener (const (void action) :: AfterInit -> Action ())

-- | Dispatch the 'Init' action.
dispatchAfterInit :: Action ()
dispatchAfterInit = dispatchEvent AfterInit

-- | Registers an action to be performed BEFORE each event phase.
beforeEveryEvent :: Action a -> Action ListenerId
beforeEveryEvent action = addListener (const (void action) :: BeforeEvent -> Action ())

beforeEveryEvent_ :: Action a -> Action ()
beforeEveryEvent_ = void . beforeEveryEvent

-- | Dispatch the 'BeforeEvent' action.
dispatchBeforeEvent :: Action ()
dispatchBeforeEvent = dispatchEvent BeforeEvent

-- | Registers an action to be performed BEFORE each render phase.
--
-- This is a good spot to add information useful to the renderer
-- since all actions have been performed. Only cosmetic changes should
-- occur during this phase.
beforeEveryRender :: Action a -> Action ListenerId
beforeEveryRender action = addListener (const (void action) :: BeforeRender -> Action ())

beforeEveryRender_ :: Action a -> Action ()
beforeEveryRender_ = void . beforeEveryRender

-- | Dispatch the 'BeforeRender' action.
dispatchBeforeRender :: Action ()
dispatchBeforeRender = dispatchEvent BeforeRender

-- | Registers an action to be performed during each render phase.
--
-- This phase should only be used by extensions which actually render something.
onEveryRender :: Action a -> Action ListenerId
onEveryRender action = addListener (const $ void action :: OnRender -> Action ())

onEveryRender_ :: Action a -> Action ()
onEveryRender_ = void . onEveryRender

-- | Dispatch the 'OnRender' action.
dispatchOnRender :: Action ()
dispatchOnRender = dispatchEvent OnRender

-- | Registers an action to be performed AFTER each render phase.
--
-- This is useful for cleaning up extension state that was registered for the
-- renderer, but needs to be cleared before the next iteration.
afterEveryRender :: Action a -> Action ListenerId
afterEveryRender action = addListener (const $ void action :: AfterRender -> Action ())

afterEveryRender_ :: Action a -> Action ()
afterEveryRender_ = void . afterEveryRender

-- | Dispatch the 'AfterRender' action.
dispatchAfterRender :: Action ()
dispatchAfterRender = dispatchEvent AfterRender

-- | Registers an action to be performed during the exit phase.
--
-- This is only triggered exactly once when the editor is shutting down. It
-- allows an opportunity to do clean-up, kill any processes you've started, or
-- save any data before the editor terminates.

onExit :: Action a -> Action ()
onExit action = void $ addListener (const $ void action :: Exit -> Action ())

-- | Dispatch the 'Exit' action.
dispatchExit :: Action ()
dispatchExit = dispatchEvent Exit

-- | Registers an action to be performed after a new buffer is added.
--
-- The supplied function will be called with a 'BufRef' to the new buffer, and the resulting 'Action' will be run.
onBufAdded :: (BufAdded -> Action result) -> Action ListenerId
onBufAdded actionF = addListener (void . actionF)

-- | Dispatch the 'BufAdded' action.
dispatchBufAdded :: BufAdded -> Action ()
dispatchBufAdded = dispatchEvent

-- | This is fired every time text in a buffer changes.
--
-- The range of text which was altered and the new value of that text are provided inside a 'BufTextChanged' event.
onBufTextChanged :: (BufTextChanged -> Action result) -> Action ListenerId
onBufTextChanged actionF = addListener (void . actionF)

-- | Dispatch the 'BufBufTextChanged' action.
dispatchBufTextChanged :: BufTextChanged -> Action ()
dispatchBufTextChanged = dispatchEvent

-- | This is a type alias to make defining your functions for use with 'asyncEventProvider' easier;
-- It represents the function your event provider function will be passed to allow dispatching
-- events. Using this type requires the @RankNTypes@ language pragma.
type Dispatcher = forall event. Typeable event => event -> IO ()

-- | This allows long-running IO processes to provide Events to Rasa asyncronously.
--
-- Don't let the type signature confuse you; it's much simpler than it seems.
--
-- Let's break it down:
--
-- Using the 'Dispatcher' type with asyncEventProvider requires the @RankNTypes@ language pragma.
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
  asyncActionProvider $ eventsToActions asyncEventProv
    where
      eventsToActions :: (Dispatcher -> IO ()) -> (Action () -> IO ()) -> IO ()
      eventsToActions aEventProv dispatcher = aEventProv (dispatcher . dispatchEvent)


-- | This function takes an IO which results in some Event, it runs the IO
-- asynchronously and dispatches the event
dispatchEventAsync :: Typeable event => IO event -> Action ()
dispatchEventAsync ioEvent = dispatchActionAsync $ dispatchEvent <$> ioEvent

-- | Event dispatcher generic over its monad
dispatchEventG :: forall m result eventType. (Monoid result, Typeable eventType, Typeable result, HasExtMonad m) => eventType -> m result
dispatchEventG evt = do
      LocalListeners _ listeners <- getExt
      results <- traverse ($ evt) (matchingListeners listeners :: [eventType -> m result])
      return $ (mconcat results :: result)

-- | addListener which is generic over its monad
addListenerG :: forall result eventType m. (Typeable eventType, Typeable result, Monoid result, HasExtMonad m) => (eventType -> m result) -> m ListenerId
addListenerG lFunc = do
  LocalListeners nextListenerId listeners  <- getExt
  let (listener, listenerId, eventType) = mkListener nextListenerId lFunc
      newListeners = M.insertWith mappend eventType [listener] listeners
  setExt $ LocalListeners (nextListenerId + 1) newListeners
  return listenerId
    where
      mkListener :: forall event r. (Typeable event, Typeable r, Monoid r) => Int -> (event -> m r) -> (Listener, ListenerId, TypeRep)
      mkListener n listenerFunc =
        let list = Listener (typeOf listenerFunc) listId listenerFunc
            listId = ListenerId n (typeRep (Proxy :: Proxy event))
            prox = typeRep (Proxy :: Proxy event)
          in (list, listId, prox)

-- | removeListener which is generic over its monad
removeListenerG :: HasExtMonad m => ListenerId -> m ()
removeListenerG listenerId@(ListenerId _ eventType) =
  overExt remover
    where
      remover (LocalListeners nextListenerId listeners) =
        let newListeners = listeners & at eventType._Just %~ filter (notMatch listenerId)
         in LocalListeners nextListenerId newListeners
      notMatch idA (Listener _ idB _) = idA /= idB

-- | Dispatches an event of any type. This should be used to define
-- your own custom event dispatchers (with more concrete types) which you can re-export.
-- You can collect results from all listeners if they were registered to return an @Action result@
-- where @result@ is a Monoid (for example a list).
dispatchEvent :: forall result eventType. (Monoid result, Typeable eventType, Typeable result) => eventType -> Action result
dispatchEvent = dispatchEventG

-- | This adds an event listener which listens for events of @eventType@ and will run the resulting
-- @Action result@ when triggered by some 'dispatchEvent'.
--
-- This should primarily be used to create your own more specific addListener functions which you re-export.
addListener :: (Typeable eventType, Typeable result, Monoid result) => (eventType -> Action result) -> Action ListenerId
addListener = addListenerG

addListener_ :: (Typeable eventType, Typeable result, Monoid result) => (eventType -> Action result) -> Action ()
addListener_ = void . addListener

-- | Removes the listener represented by the given ListenerId.
removeListener :: ListenerId -> Action ()
removeListener = removeListenerG

-- | Dispatches an event of any type to the BufAction's buffer.
-- See 'dispatchEvent'
dispatchBufEvent :: (Monoid result, Typeable eventType, Typeable result) => (eventType -> BufAction result)
dispatchBufEvent = dispatchEventG

-- | Adds a listener to the BufAction's buffer.
-- See 'addListener'
addBufListener :: (Typeable eventType, Typeable result, Monoid result) => (eventType -> BufAction result) -> BufAction ListenerId
addBufListener = addListenerG

addBufListener_ :: (Typeable eventType, Typeable result, Monoid result) => (eventType -> BufAction result) -> BufAction ()
addBufListener_ = void . addBufListener

-- | Removes a listener from the BufAction's buffer.
-- See 'removeListener'
removeBufListener :: ListenerId -> BufAction ()
removeBufListener = removeListenerG
