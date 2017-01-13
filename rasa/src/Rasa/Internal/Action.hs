{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, TemplateHaskell, StandaloneDeriving #-}

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
--      * Embed buffer actions using 'Rasa.Internal.Ext.Directive.bufDo' and 'Rasa.Internal.Ext.Directive.focusDo'
--      * Add\/Edit\/Focus buffers and a few other Editor-level things, see the 'Rasa.Internal.Ext.Directive' module.

newtype Action a = Action
  { runAct :: StateT ActionState IO a
  } deriving (Functor, Applicative, Monad, MonadState ActionState, MonadIO)


-- | Unwrap and execute an Action (returning the editor state)
execAction :: ActionState ->  Action () -> IO ActionState
execAction actionState action = execStateT (runAct action) actionState

-- | Unwrap and evaluate an Action (returning the value)
evalAction :: ActionState -> Action a -> IO a
evalAction actionState action  = evalStateT (runAct action) actionState

type AsyncAction = Async (Action ())
data ActionState = ActionState
  { _ed :: Editor
  , _asyncs :: [AsyncAction]
  , _hooks :: Hooks
  , _nextHook :: Int
  }
makeClassy ''ActionState

instance HasEditor ActionState where
  editor = ed

instance Default ActionState where
  def = ActionState
    { _ed=def
    , _asyncs=def
    , _hooks=def
    , _nextHook=0
    }

instance Show ActionState where
  show as = show (_ed as)

-- | This is a monad-transformer stack for performing actions on a specific buffer.
-- You register BufActions to be run by embedding them in a scheduled 'Action' via 'bufferDo' or 'focusDo'
--
-- Within a BufAction you can:
--
--      * Use liftIO for IO
--      * Access/edit buffer extensions; see 'bufExt'
--      * Embed and sequence any 'BufAction's from other extensions
--      * Access/Edit the buffer's 'text'
--
newtype BufAction a = BufAction
  { getBufAction::StateT Buffer IO a
  } deriving (Functor, Applicative, Monad, MonadState Buffer, MonadIO)

-- | This lifts up a bufAction into an Action which performs the 'BufAction'
-- over the referenced buffer and returns the result (if the buffer existed)
liftBuf :: BufAction a -> BufRef -> Action (Maybe a)
liftBuf bufAct (BufRef bufRef) = do
  mBuf <- use (buffers.at bufRef)
  case mBuf of
    Nothing -> return Nothing
    Just buf -> do
      (val, newBuf) <- liftIO $ flip runStateT buf . getBufAction $ bufAct
      buffers.at bufRef ?= newBuf
      return . Just $ val

