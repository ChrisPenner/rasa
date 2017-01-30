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

import Control.Lens
import Control.Monad.Free
import Control.Monad.State

import Data.Default
import Data.Map
import Data.Typeable

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
type Listeners = Map TypeRep [Listener]

-- | Free Monad Actions for Action
data ActionF state next =
  LiftState (state -> (next, state))
  | LiftIO (IO next)
  deriving (Functor)

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
  { getAction :: Free (ActionF ActionState) a
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

-- | Spawn the channels necessary to run action and do so.
bootstrapAction :: Action a -> IO a
bootstrapAction action = do
    (output, _) <- spawn unbounded
    evalAction (mkActionState output) action

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
