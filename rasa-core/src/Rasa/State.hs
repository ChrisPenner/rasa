{-# LANGUAGE TemplateHaskell, Rank2Types, ExistentialQuantification, ScopedTypeVariables  #-}
module Rasa.State where

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
  { _event :: [Event]
  , _editor :: E.Editor
  , _extState :: Map TypeRep Ext
  } deriving (Show)

makeLenses ''Store

instance Default Store where
  def =
    Store
    { _event = []
    , _editor = def
    , _extState = def
    }

focused :: Lens' Store Int
focused = editor . E.focused

buffers :: Lens' Store [Buffer]
buffers = editor.E.buffers

allBufExt :: forall a. (Show a, Typeable a) => Traversal' Store (Maybe a)
allBufExt = buffers.traverse.bufExts.at (typeRep (Proxy :: Proxy a)) . mapping coerce
  where
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext

-- Falls back on Default instance if nothing is yet stored.
-- This helps cut back on ordering and dependency concerns when initializing new buffers
bufExt ::  forall a. (Show a, Typeable a, Default a) => Lens' Buffer a
bufExt = lens getter setter
  where getter buf = fromMaybe def $
          buf^.bufExts.at (typeRep (Proxy :: Proxy a)) . mapping coerce

        setter buf new = set (bufExts.at (typeRep (Proxy :: Proxy a)) . mapping coerce) (Just new) buf
        coerce = iso (\(Ext x) -> unsafeCoerce x) Ext

exiting :: Lens' Store Bool
exiting = editor. E.exiting

ext ::  forall a. (Show a, Typeable a, Default a) => Lens' Store a
ext = lens getter setter
  where getter store = fromMaybe def $
          store^.extState.at (typeRep (Proxy :: Proxy a)) . mapping coerce

        setter store new = set (extState.at (typeRep (Proxy :: Proxy a)) . mapping coerce) (Just new) store
        coerce = iso (\(Ext x) -> unsafeCoerce x) Ext

focusedBuf :: Lens' Store Buffer
focusedBuf = lens getter setter
  where getter store = let foc = store^. focused
                        in store^?!buffers.ix foc

        setter store new = let foc = store ^. focused
                            in store & buffers.ix foc .~ new

