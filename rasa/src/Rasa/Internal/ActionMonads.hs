{-# language
   DeriveFunctor
  , GADTs
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
#-}
module Rasa.Internal.ActionMonads
  ( Action(..)
  , BufAction(..)
  , ActionF(..)
  , BufActionF(..)
  , liftActionF
  , liftBufAction
  ) where

import Rasa.Internal.Editor
import Rasa.Internal.Buffer
import Rasa.Internal.Range

import Control.Monad.Free
import Control.Monad.IO.Class

import Data.Default
import Data.Typeable

import qualified Yi.Rope as Y

-- | Free Monad Actions for Action
data ActionF next where
  LiftIO :: IO next -> ActionF next
  BufferDo :: [BufRef]  -> BufAction r -> ([r] -> next) -> ActionF next
  DispatchActionAsync :: IO (Action ()) -> next  -> ActionF next
  AsyncActionProvider :: ((Action () -> IO ()) -> IO ()) -> next  -> ActionF next
  AddBuffer :: (BufRef -> next) -> ActionF next
  GetBufRefs :: ([BufRef] -> next) -> ActionF next
  GetExt :: (Typeable ext, Show ext, Default ext) => (ext -> next) -> ActionF next
  SetExt :: (Typeable ext, Show ext, Default ext) => ext -> next -> ActionF next
  GetEditor :: (Editor -> next) -> ActionF next
  GetBuffer :: BufRef -> (Maybe Buffer -> next)  -> ActionF next
  Exit :: next -> ActionF next
  ShouldExit :: (Bool -> next) -> ActionF next
deriving instance Functor ActionF

-- | Embeds a ActionF type into the Action Monad
liftActionF :: ActionF a -> Action a
liftActionF = Action . liftF

-- | Allows running IO in BufAction.
liftFIO :: IO r -> Action r
liftFIO = liftActionF . LiftIO

instance MonadIO Action where
  liftIO = liftFIO

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
data BufActionF next where
  GetText :: (Y.YiString -> next) -> BufActionF next
  SetText :: Y.YiString -> next -> BufActionF next
  GetBufExt :: (Typeable ext, Show ext, Default ext) => (ext -> next) -> BufActionF next
  SetBufExt :: (Typeable ext, Show ext, Default ext) => ext -> next -> BufActionF next
  SetRange :: CrdRange -> Y.YiString -> next -> BufActionF next
  LiftAction :: Action r -> (r -> next) -> BufActionF next
  BufLiftIO :: IO next -> BufActionF next
deriving instance Functor BufActionF

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

-- | Embeds a BufActionF type into the BufAction Monad
liftBufAction :: BufActionF a -> BufAction a
liftBufAction = BufAction . liftF

-- | Allows running IO in BufAction.
liftBufActionFIO :: IO r -> BufAction r
liftBufActionFIO = liftBufAction . BufLiftIO

instance MonadIO BufAction where
  liftIO = liftBufActionFIO
