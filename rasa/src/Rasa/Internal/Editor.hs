{-# LANGUAGE TemplateHaskell, Rank2Types,
  ExistentialQuantification, ScopedTypeVariables,
  OverloadedStrings
  #-}

module Rasa.Internal.Editor
  (
  -- * Accessing/Storing state
  Editor
  , HasEditor
  , editor
  , buffers

-- |'buffers' is a lens into all buffers.

  , exiting

-- | 'exiting' Whether the editor is in the process of exiting. Can be set inside an 'Rasa.Internal.Action.Action':
--
-- > exiting .= True

  , ext
  , bufExt
  , nextBufId
  , range
  , BufRef(..)
  ) where

import Rasa.Internal.Buffer
import Rasa.Internal.Extensions
import Rasa.Internal.Range

import Unsafe.Coerce
import Data.Dynamic
import Data.Default
import Data.Maybe
import Data.IntMap
import Control.Lens
import qualified Yi.Rope as Y

-- | An opaque reference to a buffer (The contained Int is not meant to be
-- altered). It is possible for references to become stale if buffers are
-- deleted. Operations over invalid BufRef's are simply ignored and return
-- 'Nothing' if a value was expected.
newtype BufRef =
  BufRef Int
  deriving (Show, Eq, Ord)

-- | This is the primary state of the editor.
data Editor = Editor
  { _buffers :: IntMap Buffer
  , _exiting :: Bool
  , _extState :: ExtMap
  , _nextBufId :: Int
  }
makeClassy ''Editor

instance Show Editor where
  show ed =
    "Buffers==============\n" ++ show (ed^.buffers) ++ "\n\n"
    ++ "Editor Extensions==============\n" ++ show (ed^.extState) ++ "\n\n"
    ++ "---\n\n"


instance Default Editor where
  def =
    Editor
    { _extState = def
    , _buffers=empty
    , _exiting=False
    , _nextBufId=0
    }

-- | 'bufExt' is a lens which will focus a given extension's state within a
-- buffer (within a 'Data.Action.BufAction'). The lens will automagically focus
-- the required extension by using type inference. It's a little bit of magic,
-- if you treat the focus as a member of your extension state it should just
-- work out.
--
-- This lens falls back on the extension's 'Data.Default.Default' instance (when getting) if
-- nothing has yet been stored.

bufExt
  :: forall a.
     (Show a, Typeable a, Default a)
    => Lens' Buffer a
bufExt = lens getter setter
  where
    getter buf =
      fromMaybe def $ buf ^. bufExts . at (typeRep (Proxy :: Proxy a)) .
      mapping coerce
    setter buf new =
      set
        (bufExts . at (typeRep (Proxy :: Proxy a)) . mapping coerce)
        (Just new)
        buf
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext

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

-- | A lens over text which is encompassed by a 'Range'
range :: Range -> Lens' Buffer Y.YiString
range (Range start end) = lens getter setter
  where getter = view (text . beforeC end . afterC start)
        setter old new = old & text . beforeC end . afterC start .~ new
