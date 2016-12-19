{-# language Rank2Types #-}
module Rasa.Text where

import Control.Lens
import Yi.Rope as Y
import qualified Data.Text as T

asText :: Iso' Y.YiString T.Text
asText = iso toText fromText

row :: Int -> Lens' YiString YiString
row i = lens getter setter
  where
    getter txt = txt^?!to Y.lines.ix i
    setter new old = Y.unlines (Y.lines old & ix i .~ new)
