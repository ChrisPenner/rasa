{-# language DeriveFunctor
  , MultiParamTypeClasses
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , Rank2Types
  , ExistentialQuantification
  , TemplateHaskell #-}
module Rasa.Internal.BufAction
  ( BufAction(..)
  , getText
  , setText
  , getRange
  , setRange
  , liftState
  , liftAction
  , runBufAction
  ) where

import Rasa.Internal.Buffer
import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.Range
import Rasa.Internal.Events
import Rasa.Internal.Extensions

import Control.Arrow
import Control.Lens
import Control.Monad.Free
import Control.Monad.State

import qualified Yi.Rope as Y

-- | Contains all data about the editor; as well as a buffer which is in 'focus'.
-- We keep the full ActionState here too so that 'Action's may be lifted inside a 'BufAction'
data BufActionState = BufActionState
  { _buffer' :: Buffer
  , _ed :: Editor
  }
makeLenses ''BufActionState

instance HasBufExts BufActionState where
  bufExts = buffer'.bufExts

-- | Free Monad Actions for BufAction
data BufActionF state next =
      GetText (Y.YiString -> next)
    | SetText Y.YiString next
    | SetRange CrdRange Y.YiString next
    | forall r. LiftAction (Action r) (r -> next)
    | LiftState (state -> (next, state))
    | LiftIO (IO next)

instance Functor (BufActionF state) where
  fmap f (GetText next) = GetText (f <$> next)
  fmap f (SetText txt next) = SetText txt (f next)
  fmap f (SetRange rng txt next) = SetRange rng txt (f next)
  fmap f (LiftAction act toNext) = LiftAction act (f <$> toNext)
  fmap f (LiftState stateF) = LiftState (first f <$> stateF)
  fmap f (LiftIO ioNext) = LiftIO (f <$> ioNext)

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

-- | Allows running state actions over BufActionState; used to lift mtl state functions
liftState :: (BufActionState -> (a, BufActionState)) -> BufAction a
liftState = liftBufAction . LiftState

-- | Allows running IO in BufAction.
liftFIO :: IO r -> BufAction r
liftFIO = liftBufAction . LiftIO

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
  { getBufAction :: Free (BufActionF BufActionState) a
  } deriving (Functor, Applicative, Monad)

instance (MonadState BufActionState) BufAction where
  state = liftState

instance MonadIO BufAction where
  liftIO = liftFIO

-- | This lifts up an 'Action' to be run inside a 'BufAction'
liftAction :: Action r -> BufAction r
liftAction action = liftBufAction $ LiftAction action id

bufAt :: HasEditor e => BufRef -> Traversal' e Buffer
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
      mBuf <- preuse (bufAt bRef)
      case mBuf of
        Nothing -> return Nothing
        Just buf -> bufActionInterpreter bRef (nextF (buf^.text))

    (SetText newText next) -> do
      bufAt bRef.text .= newText
      bufActionInterpreter bRef next

    (SetRange rng newText next) -> do
      bufAt bRef.text.range rng .= newText
      dispatchEvent $ BufTextChanged rng newText
      bufActionInterpreter bRef next

    (LiftAction act toNext) -> act >>= bufActionInterpreter bRef . toNext

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
