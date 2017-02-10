{-# language ExistentialQuantification, ScopedTypeVariables #-}
module Rasa.Internal.Extensions
  ( Ext(..)
  , ExtMap
  , HasExts(..)
  , HasExtMonad(..)
  , ext
  ) where

import Control.Lens

import Data.Default
import Data.Map
import Data.Dynamic
import Data.Maybe
import Unsafe.Coerce

-- | A wrapper around an extension of any type so it can be stored in an 'ExtMap'
data Ext = forall a. Show a => Ext a
instance Show Ext where
  show (Ext a) = show a

-- | A map of extension types to their current value.
type ExtMap = Map TypeRep Ext

-- | Members of this class have access to editor extensions.
class HasExts s where
  -- | This lens focuses the Extensions States
  exts :: Lens' s (Map TypeRep Ext)

-- | This is a lens which will focus the extension state that matches the type
-- inferred as the focal point. It's a little bit of magic, if you treat the
-- focus as a member of your extension state it should just work out.
--
-- This lens falls back on the extension's 'Data.Default.Default' instance (when getting) if
-- nothing has yet been stored.

ext
  :: forall a e.
    (Show a, Typeable a, Default a, HasExts e)
  => Lens' e a
ext = lens getter setter
  where
    getter s =
      fromMaybe def $ s ^.exts . at (typeRep (Proxy :: Proxy a)) .
      mapping coerce
    setter s new =
      set
        (exts . at (typeRep (Proxy :: Proxy a)) . mapping coerce)
        (Just new)
        s
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext

class (Typeable m, Monad m) => HasExtMonad m where
  -- | Retrieve some extension state
  getExt :: (Typeable ext, Show ext, Default ext) => m ext

  -- | Set some extension state
  setExt :: (Typeable ext, Show ext, Default ext) => ext -> m ()

  -- | Set some extension state
  overExt :: (Typeable ext, Show ext, Default ext) => (ext -> ext) -> m ()
  overExt f = getExt >>= setExt . f
