{-# language OverloadedStrings #-}
module Rasa.Internal.BufActions
  ( overRange
  , replaceRange
  , deleteRange
  , insertAt
  , sizeOf
  , getLineRange
  ) where

import Rasa.Internal.BufAction
import Rasa.Internal.Range
import Rasa.Internal.Text

import Control.Lens
import qualified Yi.Rope as Y

-- | Runs function over given range of text
overRange :: CrdRange -> (Y.YiString -> Y.YiString) -> BufAction ()
overRange r f = getRange r >>= setRange r . f

-- | Deletes the text in the given range from the buffer.
deleteRange :: CrdRange -> BufAction ()
deleteRange r = replaceRange r ""

-- | Replaces the text in the given range with the given text.
replaceRange :: CrdRange -> Y.YiString -> BufAction ()
replaceRange r txt = overRange r (const txt)

-- | Inserts text into the buffer at the given 'Coord'.
insertAt :: Coord -> Y.YiString -> BufAction ()
insertAt c = replaceRange r
  where r = Range c c

-- | Rows can be represented by their line number.
type Row = Int

-- | Gets the range representing a given row (if that row exists)
getLineRange :: Row -> BufAction (Maybe CrdRange)
getLineRange n = do
  txt <- getText
  let len = txt ^? asLines . ix n . to Y.length
  return $ Range (Coord n 0) . Coord n <$> len
