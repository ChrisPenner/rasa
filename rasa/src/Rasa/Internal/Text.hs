{-# language Rank2Types #-}
module Rasa.Internal.Text
  ( asText
  , asString
  , asLines
  ) where

import Control.Lens
import qualified Yi.Rope as Y
import qualified Data.Text as T

-- | An iso which converts to/from 'Y.YiString' -> Text
asText :: Iso' Y.YiString T.Text
asText = iso Y.toText Y.fromText

-- | An iso which converts to/from 'Y.YiString' -> String
asString :: Iso' Y.YiString String
asString = iso Y.toString Y.fromString


-- | An iso which converts to/from 'Y.YiString' -> ['Y.YiString']
asLines :: Iso' Y.YiString [Y.YiString]
asLines = iso Y.lines' Y.concat
