{-# language Rank2Types #-}
module Rasa.Text
  ( asText
  , asLines
  ) where

import Control.Lens
import qualified Yi.Rope as Y
import qualified Data.Text as T

asText :: Iso' Y.YiString T.Text
asText = iso Y.toText Y.fromText

asLines :: Iso' Y.YiString [Y.YiString]
asLines = iso Y.lines Y.unlines
