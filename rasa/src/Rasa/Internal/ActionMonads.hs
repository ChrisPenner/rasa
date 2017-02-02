{-# language DeriveFunctor
  , MultiParamTypeClasses
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , RankNTypes
  , ExistentialQuantification
  , ScopedTypeVariables
#-}
module Rasa.Internal.ActionMonads
  ( Action(..)
  , BufAction(..)
  , ActionF(..)
  , BufActionF(..)
  , Listener(..)
  , Listeners
  , ListenerId(..)
  ) where

import Rasa.Internal.Editor
import Rasa.Internal.Range

import Control.Monad.Free

import Data.Default
import Data.Typeable
import qualified Data.Map as M

import qualified Yi.Rope as Y

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
data ActionF next =
    LiftIO (IO next)
  | forall r. BufferDo [BufRef] (BufAction r) ([r] -> next)
  | forall event r. Typeable event => AddListener (event -> Action r) (ListenerId -> next)
  | RemoveListener ListenerId next
  | forall event. Typeable event => DispatchEvent event next
  | DispatchActionAsync (IO (Action ())) next
  | AsyncActionProvider ((Action () -> IO ()) -> IO ()) next
  | AddBuffer (BufRef -> next)
  | GetBufRefs ([BufRef] -> next)

instance Functor ActionF where
  fmap f (LiftIO ioNext) = LiftIO $ f <$> ioNext
  fmap f (BufferDo bufRefs bufAct toNext) = BufferDo bufRefs bufAct (f <$> toNext)
  fmap f (AddListener listener next) = AddListener listener $ f <$> next
  fmap f (RemoveListener listenerId next) = RemoveListener listenerId $ f next
  fmap f (DispatchEvent event next) = DispatchEvent event (f next)
  fmap f (DispatchActionAsync ioAction next) = DispatchActionAsync ioAction (f next)
  fmap f (AsyncActionProvider actionProvider next) = AsyncActionProvider actionProvider (f next)
  fmap f (AddBuffer toNext) = AddBuffer (f <$> toNext)
  fmap f (GetBufRefs toNext) = GetBufRefs (f <$> toNext)

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
  { getAction :: Free ActionF a
  } deriving (Functor, Applicative, Monad)

-- | Free Monad Actions for BufAction
data BufActionF next =
      GetText (Y.YiString -> next)
    | SetText Y.YiString next
    | forall ext. (Typeable ext, Show ext, Default ext) => GetBufExt (ext -> next)
    | forall ext. (Typeable ext, Show ext, Default ext) => SetBufExt ext next
    | SetRange CrdRange Y.YiString next
    | forall r. LiftAction (Action r) (r -> next)
    | BufLiftIO (IO next)

instance Functor BufActionF where
  fmap f (GetText next) = GetText (f <$> next)
  fmap f (SetText txt next) = SetText txt (f next)
  fmap f (GetBufExt extToNext) = GetBufExt (f <$> extToNext)
  fmap f (SetBufExt newExt next) = SetBufExt newExt (f next)
  fmap f (SetRange rng txt next) = SetRange rng txt (f next)
  fmap f (LiftAction act toNext) = LiftAction act (f <$> toNext)
  fmap f (BufLiftIO ioNext) = BufLiftIO (f <$> ioNext)

-- | This is a monad for performing actions on a specific buffer.
-- You run 'BufAction's by embedding them in a 'Action' via 'Rasa.Internal.Actions.bufferDo' or
-- 'Rasa.Internal.Actions.buffersDo'
--
-- Within a BufAction you can:
--
--      * Use 'liftAction' to run an 'Action'
--      * Use liftIO for IO
--      * Access/Edit the buffer's text; some commands are available in "Rasa.Internal.Actions".
--      * Access/edit buffer extensions; see 'bufExt'
--      * Embed and sequence 'BufAction's from other extensions
newtype BufAction a = BufAction
  { getBufAction :: Free BufActionF a
  } deriving (Functor, Applicative, Monad)
