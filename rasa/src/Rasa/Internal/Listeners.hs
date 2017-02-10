{-# language
  GADTs
  , ExistentialQuantification
  , ScopedTypeVariables
  , RankNTypes
#-}

module Rasa.Internal.Listeners
  ( dispatchEvent
  , addListener
  , Dispatcher
  , ListenerId

  , onInit
  , dispatchInit

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
import Rasa.Internal.Events

import Control.Lens
import Control.Monad
import Data.Default
import Data.Typeable
import Data.Maybe
import qualified Data.Map as M

-- | A wrapper around event listeners so they can be stored in 'Listeners'.
data Listener where
  Listener :: Typeable eventType => ListenerId -> (eventType -> Action ()) -> Listener

-- | An opaque reverence to a specific registered event-listener.
-- A ListenerId is used only to remove listeners later with 'Rasa.Internal.Listeners.removeListener'.
data ListenerId =
  ListenerId Int TypeRep

instance Eq ListenerId where
  ListenerId a _ == ListenerId b _ = a == b

-- | A map of Event types to a list of listeners for that event
type Listeners = M.Map TypeRep [Listener]

data GlobalListeners = GlobalListeners Int Listeners

instance Show GlobalListeners where
  show _ = "Listeners"

instance Default GlobalListeners where
  def = GlobalListeners 0 M.empty

-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners :: forall eventType result. (Typeable eventType, Typeable result) => Listeners -> [eventType -> Action result]
matchingListeners listeners' = catMaybes $ getListener <$> (listeners'^.at (typeRep (Proxy :: Proxy eventType))._Just)

-- | Extract the listener function from eventType listener
getListener :: forall eventType r. (Typeable eventType, Typeable r) => Listener -> Maybe (eventType -> Action r)
getListener (Listener _ x) = cast x


onKeypress :: (Keypress -> Action ()) -> Action ListenerId
onKeypress = addListener

dispatchKeypress :: Keypress -> Action ()
dispatchKeypress = dispatchEvent

-- | This registers an event listener, as long as the listener is well-typed similar to this:
--
-- @MyEventType -> Action ()@ then it will be triggered on all dispatched events of type @MyEventType@.
-- It returns an ID which may be used with 'removeListener' to cancel the listener
-- onEveryTrigger :: Typeable event => (event -> Action b) -> Action ListenerId
-- onEveryTrigger = addListener

-- onEveryTrigger_ :: Typeable event => (event -> Action b) -> Action ()
-- onEveryTrigger_ = void . onEveryTrigger

-- | Registers an action to be performed during the Initialization phase.
--
-- This phase occurs exactly ONCE when the editor starts up.
-- Though arbitrary actions may be performed in the configuration block;
-- it's recommended to embed such actions in the onInit event listener
-- so that all event listeners are registered before anything Actions occur.
onInit :: Action a -> Action ()
onInit action = void $ addListener (const (void action) :: Init -> Action ())

dispatchInit :: Action ()
dispatchInit = dispatchEvent Init

-- | Registers an action to be performed BEFORE each event phase.
beforeEveryEvent :: Action a -> Action ListenerId
beforeEveryEvent action = addListener (const (void action) :: BeforeEvent -> Action ())

beforeEveryEvent_ :: Action a -> Action ()
beforeEveryEvent_ = void . beforeEveryEvent

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

dispatchBeforeRender :: Action ()
dispatchBeforeRender = dispatchEvent BeforeRender

-- | Registers an action to be performed during each render phase.
--
-- This phase should only be used by extensions which actually render something.
onEveryRender :: Action a -> Action ListenerId
onEveryRender action = addListener (const $ void action :: OnRender -> Action ())

onEveryRender_ :: Action a -> Action ()
onEveryRender_ = void . onEveryRender

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

dispatchAfterRender :: Action ()
dispatchAfterRender = dispatchEvent AfterRender

-- | Registers an action to be performed during the exit phase.
--
-- This is only triggered exactly once when the editor is shutting down. It
-- allows an opportunity to do clean-up, kill any processes you've started, or
-- save any data before the editor terminates.

onExit :: Action a -> Action ()
onExit action = void $ addListener (const $ void action :: Exit -> Action ())

dispatchExit :: Action ()
dispatchExit = dispatchEvent Exit

-- | Registers an action to be performed after a new buffer is added.
--
-- The supplied function will be called with a 'BufRef' to the new buffer, and the resulting 'Action' will be run.
onBufAdded :: (BufAdded -> Action a) -> Action ListenerId
onBufAdded actionF = addListener $ void <$> actionF

dispatchBufAdded :: BufAdded -> Action ()
dispatchBufAdded = dispatchEvent

-- | This is fired every time text in a buffer changes.
--
-- The range of text which was altered and the new value of that text are provided inside a 'BufTextChanged' event.
onBufTextChanged :: (BufTextChanged -> Action a) -> Action ListenerId
onBufTextChanged actionF = addListener $ void <$> actionF

dispatchBufTextChanged :: BufTextChanged -> Action ()
dispatchBufTextChanged = dispatchEvent

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
-- Using 'dispatchEvent' with asyncEventProvider requires the @RankNTypes@ language pragma.
--
-- This type as a whole represents a function which accepts a 'dispatchEvent' and returns an 'IO';
-- the dispatchEvent itself accepts data of ANY 'Typeable' type and emits it as an event (see "Rasa.Internal.Events").
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
-- > myTimer :: dispatchEvent -> IO ()
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

dispatchEvent :: forall result eventType. (Monoid result, Typeable eventType, Typeable result) => (eventType -> Action result)
dispatchEvent evt = do
      GlobalListeners _ listeners <- getExt
      results <- traverse ($ evt) (matchingListeners listeners)
      return $ mconcat results

addListener :: forall result eventType. (Typeable eventType) => (eventType -> Action result) -> Action ListenerId
addListener lFunc = do
  GlobalListeners nextListenerId listeners  <- getExt
  let (listener, listenerId, eventType) = mkListener nextListenerId lFunc
      newListeners = M.insertWith mappend eventType [listener] listeners
  setExt $ GlobalListeners (nextListenerId + 1) newListeners
  return listenerId
    where
      mkListener :: forall event r. Typeable event => Int -> (event -> Action r) -> (Listener, ListenerId, TypeRep)
      mkListener n listenerFunc =
        let list = Listener listId (void . listenerFunc)
            listId = ListenerId n (typeRep (Proxy :: Proxy event))
            prox = typeRep (Proxy :: Proxy event)
          in (list, listId, prox)

