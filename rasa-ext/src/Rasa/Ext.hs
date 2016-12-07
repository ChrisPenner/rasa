{-# LANGUAGE Rank2Types #-}

module Rasa.Ext
  ( Alteration
  , Event(..)
  , Mod(..)
  , text
  , filename
  , getExt
  , getBufExt
  , setExt
  , setBufExt
  , getState
  , getEvent
  , setEvent
  ) where

import Rasa.Alteration
import Rasa.Editor
import Rasa.Event (Event(..), Mod(..))
import Rasa.Buffer

import Data.Dynamic
import Data.Maybe

import Control.Monad.State
import Control.Lens

extract :: forall a. Typeable a => [Dynamic] -> Maybe a
extract exts =
  let mExts =
        fmap fromDynamic exts :: Typeable a =>
                                 [Maybe a]
   in case catMaybes mExts of
        [] -> Nothing
        (x:_) -> Just x

replace :: forall a. Typeable a => a -> [Dynamic] -> [Dynamic]
replace new exts = toDyn new :filter (not . isMatch) exts
  where
    isMatch :: Dynamic -> Bool
    isMatch e = typeOf e == typeOf new

getBufExt :: forall a. Typeable a => Int -> Alteration (Maybe a)
getBufExt i = extract <$> zoom (editor.buffers.ix i.bufExts) get

getExt :: forall a.  Typeable a => Alteration (Maybe a)
getExt = extract <$> zoom extState get

setExt :: Typeable a => a -> Alteration ()
setExt newExt = extState %= replace newExt

setBufExt :: forall a. Typeable a => Int -> a -> Alteration ()
setBufExt i newExt = editor.buffers.ix i.bufExts %= replace newExt

getState :: Alteration Editor
getState = zoom editor get

getEvent :: Alteration [Event]
getEvent = zoom event get

setEvent :: [Event] -> Alteration ()
setEvent = zoom event . put
