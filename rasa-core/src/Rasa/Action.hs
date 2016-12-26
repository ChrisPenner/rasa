{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification #-}

module Rasa.Action where

import Control.Monad.State
import Control.Monad.Reader

import Rasa.Buffer
import Rasa.State

-- | This is a monad-transformer stack for performing actions against the editor in response to 'Rasa.Event.Event's.
-- You register Actions to be run by embedding them in a 'Rasa.Ext.Scheduler.Scheduler' which the user then adds to their config.
--
-- Within an Action you can:
--
--      * Use liftIO for IO
--      * Access/edit extensions that are stored globally, see 'ext'
--      * Embed and sequence any 'Action's from other extensions
--      * Access the events that triggered the action, see 'events'
--      * Embed buffer actions using 'Rasa.Ext.Directive.bufDo' and 'Rasa.Ext.Directive.focusDo'
--      * Add\/Edit\/Focus buffers and a few other Editor-level things, see the 'Rasa.Ext.Directive' module.

newtype Action a = Action
  { runAct :: StateT Store (ReaderT () IO) a
  } deriving (Functor, Applicative, Monad, MonadState Store, MonadReader (), MonadIO)

execAction :: Store -> Action () -> IO Store
execAction store action = flip runReaderT () $ execStateT (runAct action) store

evalAction :: Store -> Action a -> IO a
evalAction store action = flip runReaderT () $ evalStateT (runAct action) store

-- dispatchEvent :: forall a. (Typeable a, Show a) => a -> Action ()
-- dispatchEvent evt = 

-- | This is a monad-transformer stack for performing actions on a specific buffer in response to events.
-- You register BufActions to be run by embedding them in a scheduled 'Action' via 'bufferDo' or 'focusDo'
-- which the user then adds to their config.
--
-- Within a BufAction you can:
--
--      * Use liftIO for IO
--      * Access/edit buffer extensions; see 'bufExt'
--      * Embed and sequence any 'BufAction's from other extensions
--      * Access the events that triggered the action; see 'events'
--      * Access/Edit the buffer's 'text'
--
newtype BufAction a = BufAction
  { getBufAction::StateT Buffer (ReaderT () IO) a
  } deriving (Functor, Applicative, Monad, MonadState Buffer, MonadReader (), MonadIO)

-- execBufAction :: Buffer -> BufAction a -> IO Buffer
-- execBufAction buf = flip execStateT buf . getBufAction

runBufAction :: Buffer -> BufAction a -> IO (a, Buffer)
runBufAction buf = flip runReaderT () . flip runStateT buf . getBufAction

