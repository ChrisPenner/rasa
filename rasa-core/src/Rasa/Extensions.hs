{-# language ExistentialQuantification, ScopedTypeVariables #-}
module Rasa.Extensions
  ( Ext(..)
  , ExtMap
  ) where

import Data.Map
import Data.Dynamic

data Ext = forall a. Show a => Ext a
instance Show Ext where
  show (Ext a) = show a

type ExtMap = Map TypeRep Ext
