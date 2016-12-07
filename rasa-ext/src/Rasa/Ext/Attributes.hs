module Rasa.Ext.Attributes where

import Rasa.Attributes
import Rasa.Buffer
import Control.Lens

import Data.List (insert)

-- Inserts an attribute into a buffer's attr list in sorted order
addAttr :: IAttr -> Buffer a -> Buffer a
addAttr attr = attrs %~ insert attr
