{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification #-}

module Rasa.Internal.Action where

import Control.Monad.State
import Control.Monad.Reader
import Data.Dynamic
import Data.Map

import Rasa.Internal.Buffer
import Rasa.Internal.Editor

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
  { runAct :: StateT Editor (ReaderT Hooks IO) a
  } deriving (Functor, Applicative, Monad, MonadState Editor, MonadReader Hooks, MonadIO)

-- | Unwrap and execute an Action (returning the editor state)
execAction :: Editor -> Hooks -> Action () -> IO Editor
execAction editor hooks action  = flip runReaderT hooks $ execStateT (runAct action) editor

-- | Unwrap and evaluate an Action (returning the value)
evalAction :: Editor -> Hooks -> Action a ->IO a
evalAction editor hooks action  = flip runReaderT hooks $ evalStateT (runAct action) editor

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
  { getBufAction::StateT Buffer (ReaderT Hooks IO) a
  } deriving (Functor, Applicative, Monad, MonadState Buffer, MonadReader Hooks, MonadIO)


-- | A wrapper around event listeners so they can be stored in 'Hooks'.
data Hook = forall a. Hook a

-- | A map of Event types to a list of listeners for that event
type Hooks = Map TypeRep [Hook]
