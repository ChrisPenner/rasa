{-# language Rank2Types #-}
module Rasa.Text (
  asText
  ) where

import Control.Lens
import Yi.Rope as Y
import qualified Data.Text as T

asText :: Iso' Y.YiString T.Text
asText = iso toText fromText
