{-# LANGUAGE TemplateHaskell, Rank2Types,
  ExistentialQuantification, ScopedTypeVariables #-}

module Rasa.State
  ( 
  -- * Accessing/Storing state
  Store
  , focused
  , events
  , buffers
  , extState
  , allBufExt
  , ext
  , bufExt
  , exiting
  , focusedBuf
  ) where

import Rasa.Events
import Rasa.Buffer
import qualified Rasa.Editor as E

import Unsafe.Coerce
import Data.Dynamic
import Data.Default
import Data.Map
import Data.Maybe
import Control.Lens

data Store = Store
  { _events :: [Event]
  , _editor :: E.Editor
  , _extState :: Map TypeRep Ext
  } deriving (Show)

makeLenses ''Store

instance Default Store where
  def =
    Store
    { _events = []
    , _editor = def
    , _extState = def
    }

focused :: Lens' Store Int
focused = editor . E.focused

buffers :: Lens' Store [Buffer]
buffers = editor . E.buffers

exiting :: Lens' Store Bool
exiting = editor . E.exiting

allBufExt
  :: forall a.
     (Show a, Typeable a)
  => Traversal' Store (Maybe a)
allBufExt =
  buffers . traverse . bufExts . at (typeRep (Proxy :: Proxy a)) . mapping coerce
  where
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext

-- | 'bufExt' is a lens which will focus a given extension's state within a
-- buffer (within a 'Data.Action.BufAction'). The lens will automagically focus
-- the required extension by using type inference. It's a little bit of magic,
-- if you treat the focus as a member of your extension state it should just
-- work out.
--
-- This lens falls back on the extension's 'Data.Default.Default' instance if
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
-- This lens falls back on the extension's 'Data.Default.Default' instance if
-- nothing has yet been stored.

ext
  :: forall a.
     (Show a, Typeable a, Default a)
  => Lens' Store a
ext = lens getter setter
  where
    getter store =
      fromMaybe def $ store ^. extState . at (typeRep (Proxy :: Proxy a)) .
      mapping coerce
    setter store new =
      set
        (extState . at (typeRep (Proxy :: Proxy a)) . mapping coerce)
        (Just new)
        store
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext

focusedBuf :: Lens' Store Buffer
focusedBuf = lens getter setter
  where
    getter store =
      let foc = store ^. focused
      in store ^?! buffers . ix foc
    setter store new =
      let foc = store ^. focused
      in store & buffers . ix foc .~ new
