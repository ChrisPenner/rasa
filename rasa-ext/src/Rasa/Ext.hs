{-# LANGUAGE Rank2Types #-}

module Rasa.Ext
  ( Alteration
  , Event(..)
  , Mod(..)
  , text
  , filename
  , getPlugin
  , setPlugin
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

getPlugin :: forall a.  Typeable a => Alteration (Maybe a)
getPlugin = do
  exts <- zoom extState get
  let mExts =
        fmap fromDynamic exts :: Typeable a =>
                                 [Maybe a]
   in case catMaybes mExts of
        [] -> return Nothing
        (x:_) -> return $ Just x

setPlugin :: Typeable a => a -> Alteration ()
setPlugin newExt = do
  exts <- zoom extState get
  let newExts = toDyn newExt : filter (not . isMatch) exts
  zoom extState $ put newExts
  where
    rep = typeOf newExt
    isMatch :: Dynamic -> Bool
    isMatch e = typeOf e == rep

getState :: Alteration Editor
getState = zoom editor get

getEvent :: Alteration [Event]
getEvent = zoom event get

setEvent :: [Event] -> Alteration ()
setEvent = zoom event . put
