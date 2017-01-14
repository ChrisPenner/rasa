{-# language
    TemplateHaskell
  , Rank2Types
  , ExistentialQuantification
  , ScopedTypeVariables
  , OverloadedStrings
  #-}

module Rasa.Internal.Editor
  (
  -- * Accessing/Storing state
  Editor
  , HasEditor(..)
  , buffers
  , exiting
  , ext
  , bufExt
  , nextBufId
  , BufRef(..)
  ) where

import Rasa.Internal.Buffer
import Rasa.Internal.Extensions

import Unsafe.Coerce
import Data.Dynamic
import Data.Default
import Data.Maybe
import Data.IntMap
import Control.Lens

-- | An opaque reference to a buffer (The contained Int is not meant to be
-- altered). It is possible for references to become stale if buffers are
-- deleted. Operations over invalid BufRef's are simply ignored and return
-- 'Nothing' if a value was expected.
newtype BufRef =
  BufRef Int
  deriving (Show, Eq, Ord)

-- | This is the primary state of the editor.
data Editor = Editor
  { _buffers' :: IntMap Buffer
  , _exiting' :: Bool
  , _extState' :: ExtMap
  , _nextBufId' :: Int
  }
makeLenses ''Editor

instance Show Editor where
  show ed =
    "Buffers==============\n" ++ show (ed^.buffers) ++ "\n\n"
    ++ "Editor Extensions==============\n" ++ show (ed^.extState) ++ "\n\n"
    ++ "---\n\n"

-- | This allows polymorphic lenses over anything that has access to an Editor context
class HasEditor a where
  editor :: Lens' a Editor

-- | A lens over the map of available buffers
buffers :: HasEditor e => Lens' e (IntMap Buffer)
buffers = editor.buffers'

-- | A lens over the exiting status of the editor
exiting :: HasEditor e => Lens' e Bool
exiting = editor.exiting'

-- | A lens over the extension state of 'Editor' level extensions
extState :: HasEditor e => Lens' e ExtMap
extState = editor.extState'

-- | A lens over the next buffer id to be allocated
nextBufId :: HasEditor e => Lens' e Int
nextBufId = editor.nextBufId'

instance HasEditor Editor where
  editor = lens id (flip const)

instance Default Editor where
  def =
    Editor
    { _extState'=def
    , _buffers'=empty
    , _exiting'=False
    , _nextBufId'=0
    }

-- | 'ext' is a lens which will focus the extension state that matches the type
-- inferred as the focal point. It's a little bit of magic, if you treat the
-- focus as a member of your extension state it should just work out.
--
-- This lens falls back on the extension's 'Data.Default.Default' instance (when getting) if
-- nothing has yet been stored.

ext
  :: forall a e.
    (Show a, Typeable a, Default a, HasEditor e)
  => Lens' e a
ext = lens getter setter
  where
    getter ed =
      fromMaybe def $ ed ^. extState . at (typeRep (Proxy :: Proxy a)) .
      mapping coerce
    setter ed new =
      set
        (extState . at (typeRep (Proxy :: Proxy a)) . mapping coerce)
        (Just new)
        ed
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext
