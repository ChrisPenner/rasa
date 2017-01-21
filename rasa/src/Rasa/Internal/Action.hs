{-# language DeriveFunctor
  , MultiParamTypeClasses
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , Rank2Types
  , ExistentialQuantification
  , TemplateHaskell #-}
module Rasa.Internal.Action
  ( Action(..)
  , runAction
  , evalAction
  , execAction
  , ActionState
  , Hook(..)
  , HookId(..)
  , Hooks
  , hooks
  , nextHook
  , asyncs
  ) where

import Rasa.Internal.Editor
import Rasa.Internal.Extensions

import Control.Lens
import Control.Monad.Free
import Control.Monad.State
import Control.Concurrent.Async

import Data.Default
import Data.Map
import Data.Typeable

-- | A wrapper around event listeners so they can be stored in 'Hooks'.
data Hook = forall a. Hook HookId (a -> Action ())
data HookId =
  HookId Int TypeRep

instance Eq HookId where
  HookId a _ == HookId b _ = a == b

-- | A map of Event types to a list of listeners for that event
type Hooks = Map TypeRep [Hook]

-- | Free Monad Actions for Action
data ActionF state next =
  LiftState (state -> (next, state))
  | LiftIO (IO next)
  deriving (Functor)

-- | This is a monad for performing actions against the editor.
-- You can register Actions to be run in response to events using 'Rasa.Internal.Scheduler.onEveryTrigger'
--
-- Within an Action you can:
--
--      * Use liftIO for IO
--      * Access/edit extensions that are stored globally, see 'ext'
--      * Embed any 'Action's exported other extensions
--      * Embed buffer actions using 'Rasa.Internal.Ext.Directive.bufDo' or 'Rasa.Internal.Ext.Directive.buffersDo'
--      * Add\/Edit\/Focus buffers and a few other Editor-level things, see the 'Rasa.Internal.Ext.Directive' module.
newtype Action a = Action
  { getAction :: Free (ActionF ActionState) a
  } deriving (Functor, Applicative, Monad)

type AsyncAction = Async (Action ())

-- | This contains all data representing the editor's state. It acts as the state object for an 'Action
data ActionState = ActionState
  { _ed :: Editor
  , _asyncs :: [AsyncAction]
  , _hooks :: Hooks
  , _nextHook :: Int
  }
makeLenses ''ActionState

instance Show ActionState where
  show as = show (_ed as)

instance Default ActionState where
  def = ActionState
    { _ed=def
    , _asyncs=def
    , _hooks=def
    , _nextHook=0
    }

instance HasEditor ActionState where
  editor = ed

instance HasExts ActionState where
  exts = ed.exts

-- | Embeds a ActionF type into the Action Monad
liftActionF :: ActionF ActionState a -> Action a
liftActionF = Action . liftF

-- | Allows running state actions over ActionState; used to lift mtl state functions
liftState :: (ActionState -> (a, ActionState)) -> Action a
liftState = liftActionF . LiftState

-- | Allows running IO in BufAction.
liftFIO :: IO r -> Action r
liftFIO = liftActionF . LiftIO

instance (MonadState ActionState) Action where
  state = liftState

instance MonadIO Action where
  liftIO = liftFIO

-- | Runs an Action into an IO
runAction :: ActionState -> Action a -> IO (a, ActionState)
runAction actionState (Action actionF) = actionInterpreter actionState actionF

-- | Evals an Action into an IO
evalAction :: ActionState -> Action a -> IO a
evalAction actionState action = fst <$> runAction actionState action

-- | Execs an Action into an IO
execAction :: ActionState -> Action a -> IO ActionState
execAction actionState action = snd <$> runAction actionState action

-- | Interpret the Free Monad; in this case it interprets it down to an IO
actionInterpreter :: ActionState -> Free (ActionF ActionState) r -> IO (r, ActionState)
actionInterpreter actionState (Free actionF) =
  case actionF of
    (LiftState stateFunc) -> 
      let (next, newState) = stateFunc actionState
       in actionInterpreter newState next

    (LiftIO ioNext) ->
      ioNext >>= actionInterpreter actionState

actionInterpreter actionState (Pure res) = return (res, actionState)
