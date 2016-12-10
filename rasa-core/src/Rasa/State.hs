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
import Data.Dynamic.Lens
import qualified Data.Text as T

data Ext = forall a. Ext a

data Store = Store
  { _event :: [Event]
  , _editor :: E.Editor
  , _extState :: Map TypeRep Ext
  }

makeLenses ''Store

instance Default Store where
  def =
    Store
    { _event = [def]
    , _editor = def
    , _extState = fromList []
    }


buffers :: Lens' Store [Buffer Int]
buffers = editor.E.buffers

buf :: Int -> Traversal' Store (Buffer Int)
buf bufN = editor. E.buf bufN

bufText :: Int -> Traversal' Store T.Text
bufText bufN = buf bufN.text

bufExt :: Typeable a => Int -> Traversal' Store a
bufExt bufN = buf bufN.bufExts.traverse._Dynamic

exiting :: Lens' Store Bool
exiting = editor. E.exiting

ext ::  forall a. Typeable a => Lens' Store (Maybe a)
ext = extState . at (typeRep (Proxy :: Proxy a)) . mapping coerce
  where
    coerce = iso (\(Ext x) -> unsafeCoerce x) Ext
