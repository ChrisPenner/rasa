{-# language
     Rank2Types
  , TemplateHaskell
  , OverloadedStrings
  , ExistentialQuantification
  , ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  , StandaloneDeriving
  #-}

module Rasa.Internal.Buffer
  ( Buffer
  , HasBuffer(..)
  , text
  , getText
  , bufExt
  , Ext(..)
  , mkBuffer
  ) where

import Rasa.Internal.Extensions

import qualified Yi.Rope as Y
import Control.Lens hiding (matching)
import Data.Map
import Data.Typeable
import Data.Default
import Data.Maybe
import Unsafe.Coerce

-- | A buffer, holds the text in the buffer and any extension states that are set on the buffer.
data Buffer = Buffer
  { _text' :: Y.YiString
  , _bufExts' :: ExtMap
  }
makeLenses ''Buffer

instance Show Buffer where
  show b = "<Buffer {text:" ++ show (b^..text . to (Y.take 30)) ++ "...,\n"
           ++ "exts: " ++ show (b^.bufExts) ++ "}>\n"

-- | This allows creation of polymorphic lenses over any type which has access to a Buffer
class HasBuffer a where
  buffer :: Lens' a Buffer

instance HasBuffer Buffer where
  buffer = lens id (flip const)

-- | This lens focuses the text of the in-scope buffer.
text :: HasBuffer b => Lens' b Y.YiString
text = buffer.text'

-- | This getter-lens focuses the text of the in-scope buffer.
getText :: HasBuffer b => Getting r b Y.YiString
getText = buffer.text'
  
-- | This lens focuses the Extensions States map of the in-scope buffer.
bufExts :: HasBuffer b => Lens' b ExtMap
bufExts = buffer.bufExts'

-- | 'bufExt' is a lens which will focus a given extension's state within a
-- buffer (within a 'Data.Action.BufAction'). The lens will automagically focus
-- the required extension by using type inference. It's a little bit of magic,
-- if you treat the focus as a member of your extension state it should just
-- work out.
--
-- This lens falls back on the extension's 'Data.Default.Default' instance (when getting) if
-- nothing has yet been stored.

bufExt
  :: forall a s.
    (Show a, Typeable a, Default a, HasBuffer s)
    => Lens' s a
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

-- | Creates a new buffer from the given text.
mkBuffer :: Y.YiString -> Buffer
mkBuffer txt =
  Buffer
    { _text' = txt
    , _bufExts' = empty
    }
