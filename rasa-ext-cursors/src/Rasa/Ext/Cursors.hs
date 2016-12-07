module Rasa.Ext.Cursors (getCursor) where

import Rasa.Ext

import Data.Typeable
import Data.Maybe

newtype Cursor = Cursor Int
  deriving (Show, Typeable)

getCursor :: Int -> Alteration Int
getCursor bufI = do
  Cursor i <- fromMaybe (Cursor 0) <$> getBufExt bufI
  return i
