{-# language
    TemplateHaskell
#-}

module Rasa.Internal.Interpreters
  ( runAction
  , evalAction
  , execAction
  , bootstrapAction
  , mkActionState

  , runBufAction
  ) where

import Rasa.Internal.ActionMonads
import Rasa.Internal.Buffer
import Rasa.Internal.Editor
import Rasa.Internal.Events hiding (Exit)
import Rasa.Internal.Extensions
import Rasa.Internal.Listeners
import Rasa.Internal.Range

import Control.Monad.Free
import Control.Monad.State
import Control.Lens
import Data.Default
import Data.Maybe
import qualified Data.IntMap as IM

import Pipes hiding (Proxy, next)
import Pipes.Concurrent hiding (Buffer)

-- | This contains all data representing the editor's state. It acts as the state object for an 'Action
data ActionState = ActionState
  { _ed :: Editor
  , _nextBufId :: Int
  , _actionQueue :: Output (Action ())
  }
makeLenses ''ActionState

instance Show ActionState where
  show as = show (_ed as)

mkActionState :: Output (Action ()) -> ActionState
mkActionState out = ActionState
    { _ed=def
    , _nextBufId=0
    , _actionQueue=out
    }

instance HasEditor ActionState where
  editor = ed

instance HasExts ActionState where
  exts = ed.exts

-- | Runs an Action into an IO
runAction :: ActionState -> Action a -> IO (a, ActionState)
runAction actionState (Action actionF) = flip runStateT actionState $ actionInterpreter actionF

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
actionInterpreter :: Free ActionF r -> StateT ActionState IO r
actionInterpreter (Free actionF) =
  case actionF of
    (LiftIO ioNext) ->
      liftIO ioNext >>= actionInterpreter

    (BufferDo bufRefs bufAct toNext) -> do
      results <- forM bufRefs $ \(BufRef bInd) ->
        use (buffers.at bInd) >>= traverse (handleBuf bInd)
      actionInterpreter . toNext $ catMaybes results
        where handleBuf bIndex buf = do
                let Action act = runBufAction bufAct buf
                (res, newBuffer) <- actionInterpreter act
                buffers.at bIndex ?= newBuffer
                return res

    (DispatchActionAsync asyncActionIO next) -> do
      asyncQueue <- use actionQueue
      let effect = (liftIO asyncActionIO >>= yield) >-> toOutput asyncQueue
      liftIO . void . forkIO $ runEffect effect >> performGC
      actionInterpreter next

    (AsyncActionProvider dispatcherToIO next) -> do
      asyncQueue <- use actionQueue
      let dispatcher action =
            let effect = yield action >-> toOutput asyncQueue
             in void . forkIO $ runEffect effect >> performGC
      liftIO . void . forkIO $ dispatcherToIO dispatcher
      actionInterpreter next

    (AddBuffer txt toNext) -> do
      bufId <- nextBufId <+= 1
      let bufRef = BufRef bufId
      buffers.at bufId ?= mkBuffer txt bufRef
      let Action dBufAdded = dispatchBufAdded (BufAdded bufRef)
      actionInterpreter (dBufAdded >> toNext bufRef)

    (GetBufRefs toNext) ->
      use (buffers.to IM.keys) >>= actionInterpreter . toNext . fmap BufRef

    (GetExt toNext) ->
      use ext >>= actionInterpreter . toNext

    (SetExt new next) -> do
      ext .= new
      actionInterpreter next

    (GetEditor toNext) ->
      use ed >>= actionInterpreter . toNext

    (GetBuffer (BufRef bufInd) toNext) ->
      use (buffers.at bufInd) >>= actionInterpreter . toNext

    (Exit next) -> do
      exiting .= True
      actionInterpreter next

    (ShouldExit toNext) -> do
      ex <- use exiting
      actionInterpreter (toNext ex)

actionInterpreter (Pure res) = return res

-- | This lifts up a bufAction into an Action which performs the 'BufAction'
-- over the referenced buffer and returns the result (if the buffer existed)
runBufAction :: BufAction a -> Buffer -> Action (a, Buffer)
runBufAction (BufAction bufActF) buf = flip runStateT buf $ bufActionInterpreter bufActF

-- | Interpret the Free Monad; in this case it interprets it down to an Action.
bufActionInterpreter :: Free BufActionF r -> StateT Buffer Action r
bufActionInterpreter (Free bufActionF) =
  case bufActionF of
    (GetText nextF) -> use text >>= bufActionInterpreter . nextF

    (SetText newText next) -> do
      text .= newText
      bufActionInterpreter next

    (GetBufRef toNext) -> do
      bref <- use ref
      bufActionInterpreter $ toNext bref


    (SetRange rng newText next) -> do
      text.range rng .= newText
      lift . dispatchBufTextChanged $ BufTextChanged rng newText
      bufActionInterpreter next

    (LiftAction act toNext) -> lift act >>= bufActionInterpreter . toNext

    (GetBufExt extToNext) ->
      use ext >>= bufActionInterpreter . extToNext

    (SetBufExt new next) -> do
      ext .= new
      bufActionInterpreter next

    (BufLiftIO ioNext) ->
      liftIO ioNext >>= bufActionInterpreter

bufActionInterpreter (Pure res) = return res
