{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, TemplateHaskell, StandaloneDeriving,
   MultiParamTypeClasses, FlexibleInstances #-}

module Rasa.Internal.Action
  ( Action
  , runAct
  , ActionState
  , Hook(..)
  , HookId(..)
  , Hooks
  , hooks
  , nextHook
  , asyncs
  , evalAction
  , execAction
  , runAction
  ) where

import Control.Lens
import Control.Concurrent.Async
import Control.Monad.State
import Data.Dynamic
import Data.Map
import Data.Default

import Rasa.Internal.Editor
import Rasa.Internal.Extensions


-- | A wrapper around event listeners so they can be stored in 'Hooks'.
data Hook = forall a. Hook HookId (a -> Action ())
data HookId =
  HookId Int TypeRep

instance Eq HookId where
  HookId a _ == HookId b _ = a == b

-- | A map of Event types to a list of listeners for that event
type Hooks = Map TypeRep [Hook]

-- | This is a monad-transformer stack for performing actions against the editor.
-- You register Actions to be run in response to events using 'Rasa.Internal.Scheduler.onEveryTrigger'
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

-- | Run an Action (returning the value)
runAction :: Action a -> ActionState -> IO (a, ActionState)
runAction action = runStateT (runAct action)

type AsyncAction = Async (Action ())

-- | This contains all data representing the editor's state. It acts as the state object for an 'Action
data ActionState = ActionState
  { _ed :: Editor
  , _asyncs :: [AsyncAction]
  , _hooks :: Hooks
  , _nextHook :: Int
  }
makeLenses ''ActionState

instance HasEditor ActionState where
  editor = ed

instance HasExts ActionState where
  exts = ed.exts

instance Default ActionState where
  def = ActionState
    { _ed=def
    , _asyncs=def
    , _hooks=def
    , _nextHook=0
    }

instance Show ActionState where
  show as = show (_ed as)
