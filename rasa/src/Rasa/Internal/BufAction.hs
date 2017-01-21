{-# language DeriveFunctor
  , MultiParamTypeClasses
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , Rank2Types
  , TemplateHaskell #-}
module Rasa.Internal.BufAction
  ( BufAction(..)
  , getText
  , setText
  , getRange
  , setRange
  , overRange
  , liftState
  , liftAction
  , runBufAction
  ) where

import Rasa.Internal.Buffer
import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.Range
import Rasa.Internal.Scheduler
import Rasa.Internal.Events
import Rasa.Internal.Extensions

import Control.Lens
import Control.Monad.Free
import Control.Monad.State

import qualified Yi.Rope as Y

-- | Contains all data about the editor; as well as a buffer which is in 'focus'.
-- We keep the full ActionState here too so that 'Action's may be lifted inside a 'BufAction'
data BufActionState = BufActionState
  { _buffer' :: Buffer
  , _actionState :: ActionState
  }
makeLenses ''BufActionState

instance HasBufExts BufActionState where
  bufExts = buffer'.bufExts

-- | Free Monad Actions for BufAction
data BufActionF state next =
      GetText (Y.YiString -> next)
    | SetText Y.YiString next
    | SetRange CrdRange Y.YiString next
    | LiftState (state -> (next, state))
    | LiftIO (IO next)
  deriving (Functor)

-- | Embeds a BufActionF type into the BufAction Monad
liftBufAction :: BufActionF BufActionState a -> BufAction a
liftBufAction = BufAction . liftF

-- | Returns the text of the current buffer
getText :: BufAction Y.YiString
getText = liftBufAction $ GetText id

-- | Sets the text of the current buffer
setText :: Y.YiString -> BufAction ()
setText txt = liftBufAction $ SetText txt ()

-- | Gets the range of text from the buffer
getRange :: CrdRange -> BufAction Y.YiString
getRange rng = view (range rng) <$> getText

-- | Sets the range of text from the buffer
setRange :: CrdRange -> Y.YiString -> BufAction ()
setRange rng txt = liftBufAction $ SetRange rng txt ()

-- | Runs function over given range of text
overRange :: CrdRange -> (Y.YiString -> Y.YiString) -> BufAction ()
overRange r f = getRange r >>= setRange r . f

-- | Allows running state actions over BufActionState; used to lift mtl state functions
liftState :: (BufActionState -> (a, BufActionState)) -> BufAction a
liftState = liftBufAction . LiftState

-- | Allows running IO in BufAction.
liftFIO :: IO r -> BufAction r
liftFIO = liftBufAction . LiftIO

-- | This is a monad for performing actions on a specific buffer.
-- You run 'BufAction's by embedding them in a 'Action' via 'bufferDo' or 'buffersDo'
--
-- Within a BufAction you can:
--
--      * Use 'liftAction' to run an 'Action'; It is your responsibility to ensure that any nested 'Action's don't edit
  --      the Buffer which the current 'BufAction' is editing; behaviour is undefined if this occurs.
--      * Use liftIO for IO
--      * Access/Edit the buffer's text
--      * Access/edit buffer extensions; see 'bufExt'
--      * Embed and sequence 'BufAction's from other extensions
newtype BufAction a = BufAction
  { getBufAction :: Free (BufActionF BufActionState) a
  } deriving (Functor, Applicative, Monad)

instance (MonadState BufActionState) BufAction where
  state = liftState

instance MonadIO BufAction where
  liftIO = liftFIO

-- | This lifts up an 'Action' to be run inside a 'BufAction'
liftAction :: Action r -> BufAction r
liftAction action = do
  actState <- use actionState
  (res, endState) <- liftIO $ runAction actState action
  actionState .= endState
  return res

bufAt :: BufRef -> Traversal' ActionState Buffer
bufAt (BufRef bufInd) = buffers.at bufInd._Just

-- | This lifts up a bufAction into an Action which performs the 'BufAction'
-- over the referenced buffer and returns the result (if the buffer existed)
runBufAction :: BufAction a -> BufRef -> Action (Maybe a)
runBufAction (BufAction bufActF) = flip bufActionInterpreter bufActF

-- | Interpret the Free Monad; in this case it interprets it down to an Action.
bufActionInterpreter :: BufRef -> Free (BufActionF BufActionState) r -> Action (Maybe r)
bufActionInterpreter bRef (Free bufActionF) =
  case bufActionF of
    (GetText nextF) -> do
      actState <- get
      case actState^? bufAt bRef of
        Nothing -> return Nothing
        Just buf -> bufActionInterpreter bRef (nextF (buf^.text))

    (SetText newText next) -> do
      bufAt bRef.text .= newText
      bufActionInterpreter bRef next

    (SetRange rng newText next) -> do
      bufAt bRef.text.range rng .= newText
      dispatchEvent $ BufTextChanged rng newText
      bufActionInterpreter bRef next

    (LiftState stateFunc) -> do
      mBuf <- preuse (bufAt bRef)
      case mBuf of
        Nothing -> return Nothing
        Just buf -> do
          actState <- get
          let (next, BufActionState newBuf newActState) = stateFunc (BufActionState buf actState)
          put newActState
          bufAt bRef .= newBuf
          bufActionInterpreter bRef next

    (LiftIO ioNext) ->
      liftIO ioNext >>= bufActionInterpreter bRef

bufActionInterpreter _ (Pure res) = return $ Just res
