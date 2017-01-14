{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, TemplateHaskell, StandaloneDeriving,
   MultiParamTypeClasses, FlexibleInstances #-}

module Rasa.Internal.Action where

import Control.Lens
import Control.Concurrent.Async
import Control.Monad.State
import Data.Dynamic
import Data.Map
import Data.Default

import Rasa.Internal.Buffer
import Rasa.Internal.Editor


-- | A wrapper around event listeners so they can be stored in 'Hooks'.
data Hook = forall a. Hook HookId a
data HookId =
  HookId Int TypeRep

instance Eq HookId where
  HookId a _ == HookId b _ = a == b

-- | A map of Event types to a list of listeners for that event
type Hooks = Map TypeRep [Hook]

-- | This is a monad-transformer stack for performing actions against the editor.
-- You register Actions to be run in response to events using 'Rasa.Internal.Scheduler.eventListener'
--
-- Within an Action you can:
--
--      * Use liftIO for IO
--      * Access/edit extensions that are stored globally, see 'ext'
--      * Embed any 'Action's exported other extensions
--      * Embed buffer actions using 'Rasa.Internal.Ext.Directive.bufDo' or 'Rasa.Internal.Ext.Directive.buffersDo'
--      * Add\/Edit\/Focus buffers and a few other Editor-level things, see the 'Rasa.Internal.Ext.Directive' module.

newtype Action a = Action
  { runAct :: StateT ActionState IO a
  } deriving (Functor, Applicative, Monad, MonadState ActionState, MonadIO)

-- | Execute an Action (returning the editor state)
execAction :: ActionState ->  Action () -> IO ActionState
execAction actionState action = execStateT (runAct action) actionState

-- | Evaluate an Action (returning the value)
evalAction :: ActionState -> Action a -> IO a
evalAction actionState (Action action)  = evalStateT action actionState

type AsyncAction = Async (Action ())
-- | This contains all data representing the editor's state. It acts as the state object for an 'Action
data ActionState = ActionState
  { _ed :: Editor
  , _asyncs :: [AsyncAction]
  , _hooks :: Hooks
  , _nextHook :: Int
  }
makeLenses ''ActionState

-- | Allows polymorphic lenses which need to access something in ActionState
class HasActionState a where
  actionState :: Lens' a ActionState

instance HasActionState ActionState where
  actionState = lens id (flip const)

instance HasEditor ActionState where
  editor = ed

instance Default ActionState where
  def = ActionState
    { _ed=def
    , _asyncs=def
    , _hooks=def
    , _nextHook=0
    }

-- | Contains all data about the editor; as well as a buffer which is in 'focus'.
-- We keep the full ActionState here too so that 'Action's may be lifted inside a 'BufAction'
data BufActionState = BufActionState
  { _actState :: ActionState
  , _buf :: Buffer
  }
makeLenses ''BufActionState

instance Show ActionState where
  show as = show (_ed as)

-- | This is a monad-transformer stack for performing actions on a specific buffer.
-- You run 'BufAction's by embedding them in a 'Action' via 'bufferDo' or 'buffersDo'
--
-- Within a BufAction you can:
--
--      * Use 'liftAction' to run an 'Action'; It is your responsibility to ensure that any nested 'Action's don't edit
  --      the Buffer which the current 'BufAction' is editing; behaviour is undefined if this occurs.
--      * Use liftIO for IO
--      * Access/Edit the buffer's 'text'
--      * Access/edit buffer extensions; see 'bufExt'
--      * Embed and sequence 'BufAction's from other extensions

newtype BufAction a = BufAction
  { runBufAct :: StateT BufActionState IO a
  } deriving (Functor, Applicative, Monad, MonadState BufActionState, MonadIO)

instance HasBuffer BufActionState where
  buffer = buf

instance HasActionState BufActionState where
  actionState = actState

-- | This lifts up a bufAction into an Action which performs the 'BufAction'
-- over the referenced buffer and returns the result (if the buffer existed)
liftBuf :: BufAction a -> BufRef -> Action (Maybe a)
liftBuf (BufAction bufAct) (BufRef bufInd) = do
  actionState' <- get
  mBuf <- getBuffer
  case mBuf of
    Nothing -> return Nothing
    Just b -> do
      let bufActSt = BufActionState actionState' b
      (val, newBufActState) <- liftIO $ runStateT bufAct bufActSt
      put (newBufActState^.actionState)
      setBuffer (newBufActState^.buf)
      return (Just val)
  where
    getBuffer = use (buffers.at bufInd)
    setBuffer b = buffers.at bufInd ?= b

-- | This lifts up an 'Action' to be run inside a 'BufAction'
--
-- it is your responsibility to ensure that any nested 'Action's don't edit
-- the Buffer which the current 'BufAction' is editing; behaviour is undefined if this occurs.
liftAction :: Action a -> BufAction a
liftAction (Action action) = BufAction $ zoom actionState action
