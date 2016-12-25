{-# language Rank2Types #-}
module Rasa.Text
  ( asText
  -- , range
  -- , beforeC
  -- , afterC
  , asLines
  ) where

import Control.Lens
import qualified Yi.Rope as Y
import qualified Data.Text as T

asText :: Iso' Y.YiString T.Text
asText = iso Y.toText Y.fromText

asLines :: Iso' Y.YiString [Y.YiString]
asLines = iso Y.lines Y.unlines


-- oRange :: (Offset, Offset) -> Lens' Y.YiString  Y.YiString
-- oRange (Offset start, Offset end) = lens getter setter
--   where
--     getter = Y.drop start . Y.take end
--     setter old new = let prefix = Y.take start old
--                          suffix = Y.drop end old
--                       in prefix <> new <> suffix


