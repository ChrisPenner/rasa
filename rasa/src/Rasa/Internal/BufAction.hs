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
  ) where

import Rasa.Internal.ActionMonads
import Rasa.Internal.Range

import Control.Lens
import Data.Default
import Data.Typeable

import qualified Yi.Rope as Y

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
getBufExt :: (Typeable ext, Show ext, Default ext) => BufAction ext
getBufExt = liftBufAction $ GetBufExt id

-- | Set some buffer extension state
setBufExt :: (Typeable ext, Show ext, Default ext) => ext -> BufAction ()
setBufExt newExt = liftBufAction $ SetBufExt newExt ()

-- | Set some buffer extension state
overBufExt :: (Typeable ext, Show ext, Default ext) => (ext -> ext) -> BufAction ()
overBufExt f = getBufExt >>= setBufExt . f

-- | This lifts up an 'Action' to be run inside a 'BufAction'
liftAction :: Action r -> BufAction r
liftAction action = liftBufAction $ LiftAction action id
