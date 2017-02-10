{-# language
    Rank2Types
  , OverloadedStrings
  , GADTs
  , ScopedTypeVariables
#-}
module Rasa.Internal.Actions
  (
  -- * Performing Actions on Buffers
    bufDo
  , bufDo_
  , buffersDo
  , buffersDo_

  -- * Editor Actions
  , exit
  , newBuffer
  , getBufRefs
  , nextBufRef
  , prevBufRef

  , mkRegistrar
  , mkDispatcher
  , ListenerId
  ) where

import Rasa.Internal.Editor
import Rasa.Internal.Action hiding (Listener, Listeners, ListenerId, listeners, nextListenerId)
import Rasa.Internal.BufAction
import Rasa.Internal.Events

import Control.Monad
import Control.Lens
import Data.Default
import Data.Maybe
import Data.Typeable

import qualified Data.Map as M
import qualified Yi.Rope as Y


-- | This lifts a 'Rasa.Action.BufAction' to an 'Rasa.Action.Action' which
-- performs the 'Rasa.Action.BufAction' on every buffer and collects the return
-- values as a list.

buffersDo :: BufAction a -> Action [a]
buffersDo bufAct = do
  bufRefs <- getBufRefs
  bufferDo bufRefs bufAct

buffersDo_ :: BufAction a -> Action ()
buffersDo_ = void . buffersDo

-- | This lifts a 'Rasa.Internal.Action.BufAction' to an 'Rasa.Internal.Action.Action' which
-- performs the 'Rasa.Internal.Action.BufAction' on the buffer referred to by the 'BufRef'
-- If the buffer referred to no longer exists this returns: @Nothing@.
bufDo :: BufRef -> BufAction a -> Action (Maybe a)
bufDo bufRef bufAct = listToMaybe <$> bufferDo [bufRef] bufAct

bufDo_ :: BufRef -> BufAction a -> Action ()
bufDo_ bufRef bufAct = void $ bufDo bufRef bufAct

-- | This adds a new buffer with the given text, returning a reference to that buffer.
newBuffer :: Y.YiString -> Action BufRef
newBuffer txt = do
  bufRef <- addBuffer
  void $ bufferDo [bufRef] (setText txt)
  dispatchEvent (BufAdded bufRef)
  return bufRef

-- | Gets 'BufRef' that comes after the one provided
nextBufRef :: BufRef -> Action BufRef
nextBufRef br = do
  bufRefs <- getBufRefs
  return $ if null bufRefs
              then br
              else case dropWhile (<= br) bufRefs of
                     [] -> head bufRefs
                     (x:_) -> x

-- | Gets 'BufRef' that comes before the one provided
prevBufRef :: BufRef -> Action BufRef
prevBufRef br = do
  bufRefs <- getBufRefs
  return $ if null bufRefs
              then br
              else case dropWhile (>= br) (reverse bufRefs) of
                     [] -> last bufRefs
                     (x:_) -> x



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

data L = L Int Listeners

instance Show L where
  show _ = "Listeners"

instance Default L where
  def = L 0 M.empty

mkDispatcher :: forall result eventType. (Monoid result, Typeable eventType, Typeable result) => (eventType -> Action result)
mkDispatcher = dispatcher
  where
    dispatcher :: eventType -> Action result
    dispatcher evt = do
      L _ listeners <- getExt
      results <- traverse ($ evt) (matchingListeners listeners)
      return $ mconcat results

mkRegistrar :: forall result eventType. (Typeable eventType) => (eventType -> Action result) -> Action ListenerId
mkRegistrar = registrar
  where
    registrar :: (eventType -> Action result) -> Action ListenerId
    registrar lFunc = do
      L nextListenerId listeners  <- getExt
      let (listener, listenerId, eventType) = mkListener nextListenerId lFunc
          newListeners = M.insertWith mappend eventType [listener] listeners
      setExt $ L (nextListenerId + 1) newListeners
      return listenerId

    mkListener :: forall event r. Typeable event => Int -> (event -> Action r) -> (Listener, ListenerId, TypeRep)
    mkListener n listenerFunc =
      let list = Listener listId (void . listenerFunc)
          listId = ListenerId n (typeRep (Proxy :: Proxy event))
          prox = typeRep (Proxy :: Proxy event)
        in (list, listId, prox)

-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners :: forall eventType result. (Typeable eventType, Typeable result) => Listeners -> [eventType -> Action result]
matchingListeners listeners' = catMaybes $ getListener <$> (listeners'^.at (typeRep (Proxy :: Proxy eventType))._Just)

-- | Extract the listener function from eventType listener
getListener :: forall eventType r. (Typeable eventType, Typeable r) => Listener -> Maybe (eventType -> Action r)
getListener (Listener _ x) = cast x
