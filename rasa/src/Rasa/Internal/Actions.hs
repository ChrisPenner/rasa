{-# language
    Rank2Types
  , OverloadedStrings
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
  , getBufRefs
  , nextBufRef
  , prevBufRef
  ) where

import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.BufAction

import Control.Monad
import Data.Maybe

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
