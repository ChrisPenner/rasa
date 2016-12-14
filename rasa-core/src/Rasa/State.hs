{-# LANGUAGE TemplateHaskell, Rank2Types, ExistentialQuantification, ScopedTypeVariables  #-}
module Rasa.State where

import Rasa.Event
import Rasa.Buffer
import qualified Rasa.Editor as E

import Unsafe.Coerce
import Data.Dynamic
import Data.Default
import Data.Map
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
    { _event = [def]
    , _editor = def
    , _extState = empty
    }

focused :: Lens' Store Int
focused = editor . E.focused

buffers :: Lens' Store [Buffer]
buffers = editor.E.buffers

buf :: Int -> Traversal' Store Buffer
buf bufN = editor. E.buf bufN

allBufExt :: forall a. (Show a, Typeable a) => Traversal' Store (Maybe a)
allBufExt = buffers.traverse.bufExts.at (typeRep (Proxy :: Proxy a)) . mapping coerce
  where
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext

bufExt ::  forall a. (Show a, Typeable a) => Lens' Buffer (Maybe a)
bufExt = bufExts.at (typeRep (Proxy :: Proxy a)) . mapping coerce
  where
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext

exiting :: Lens' Store Bool
exiting = editor. E.exiting

ext ::  forall a. (Show a, Typeable a) => Lens' Store (Maybe a)
ext = extState . at (typeRep (Proxy :: Proxy a)) . mapping coerce
  where
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext

focusedBuf :: Lens' Store Buffer
focusedBuf = lens getter setter
  where getter store = let foc = store^. focused
                        in store^?!buffers.ix foc

        setter store new = let foc = store ^. focused
                            in store & buffers.ix foc .~ new

