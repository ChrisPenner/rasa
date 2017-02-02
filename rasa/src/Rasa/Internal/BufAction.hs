{-# language DeriveFunctor
  , MultiParamTypeClasses
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , Rank2Types
  , ExistentialQuantification
  , TemplateHaskell
#-}
module Rasa.Internal.BufAction
  ( BufAction(..)
  , getText
  , setText
  , getRange
  , setRange
  , getBufExt
  , setBufExt
  , overBufExt
  , liftAction
  , runBufAction
  ) where

import Rasa.Internal.ActionMonads
import Rasa.Internal.Buffer
import Rasa.Internal.Editor
import Rasa.Internal.Action
import Rasa.Internal.Range
import Rasa.Internal.Events
import Rasa.Internal.Extensions

import Control.Lens
import Control.Monad.Free
import Control.Monad.State
import Data.Default
import Data.Typeable

import qualified Yi.Rope as Y

-- | Embeds a BufActionF type into the BufAction Monad
liftBufAction :: BufActionF a -> BufAction a
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

-- | Retrieve some buffer extension state
getBufExt :: forall ext. (Typeable ext, Show ext, Default ext) => BufAction ext
getBufExt = liftBufAction $ GetBufExt id

-- | Set some buffer extension state
setBufExt :: forall ext. (Typeable ext, Show ext, Default ext) => ext -> BufAction ()
setBufExt newExt = liftBufAction $ SetBufExt newExt ()

-- | Set some buffer extension state
overBufExt :: forall ext. (Typeable ext, Show ext, Default ext) => (ext -> ext) -> BufAction ()
overBufExt f = getBufExt >>= setBufExt . f

-- | Allows running IO in BufAction.
liftFIO :: IO r -> BufAction r
liftFIO = liftBufAction . BufLiftIO

newtype BufExts = BufExts
  { _bExts :: ExtMap
  }
makeLenses ''BufExts

instance HasBufExts BufExts where
  bufExts = bExts

instance MonadIO BufAction where
  liftIO = liftFIO

-- | This lifts up an 'Action' to be run inside a 'BufAction'
liftAction :: Action r -> BufAction r
liftAction action = liftBufAction $ LiftAction action id

bufAt :: HasEditor e => BufRef -> Traversal' e Buffer
bufAt (BufRef bufInd) = buffers.at bufInd._Just

-- | This lifts up a bufAction into an Action which performs the 'BufAction'
-- over the referenced buffer and returns the result (if the buffer existed)
runBufAction :: BufAction a -> StateT Buffer Action a
runBufAction (BufAction bufActF) = bufActionInterpreter bufActF

-- | Interpret the Free Monad; in this case it interprets it down to an Action.
bufActionInterpreter :: Free BufActionF r -> StateT Buffer Action r
bufActionInterpreter (Free bufActionF) =
  case bufActionF of
    (GetText nextF) -> use text >>= bufActionInterpreter . nextF

    (SetText newText next) -> do
      text .= newText
      bufActionInterpreter next

    (SetRange rng newText next) -> do
      text.range rng .= newText
      lift . dispatchEvent $ BufTextChanged rng newText
      bufActionInterpreter next

    (LiftAction act toNext) -> lift act >>= bufActionInterpreter . toNext

    (GetBufExt extToNext) ->
      use bufExt >>= bufActionInterpreter . extToNext

    (SetBufExt new next) -> do
      bufExt .= new
      bufActionInterpreter next

    (BufLiftIO ioNext) ->
      liftIO ioNext >>= bufActionInterpreter

bufActionInterpreter (Pure res) = return res
