{-# language ExistentialQuantification, ScopedTypeVariables #-}
module Rasa.Internal.Extensions
  ( Ext(..)
  , ExtMap
  ) where

import Data.Map
import Data.Dynamic

-- | A wrapper around an extension of any type so it can be stored in an 'ExtMap'
data Ext = forall a. Show a => Ext a
instance Show Ext where
  show (Ext a) = show a

-- | A map of extension types to their current value.
type ExtMap = Map TypeRep Ext
