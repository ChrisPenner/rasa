{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors.Internal.Base
  ( rangeDo
  , rangeDo_
  , overRanges
  , getRanges
  , setRanges
  , overEachRange
  , addRange
  , displayRange
  ) where


import Rasa.Ext
import Rasa.Ext.Style

import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.List
import Data.Default
import qualified Yi.Rope as Y

-- | Stores the cursor ranges in each buffer.
data Cursors = Cursors
  { _cursors :: [CrdRange]
  } deriving (Typeable, Show)
makeLenses ''Cursors

instance Default Cursors where
  def = Cursors {
  _cursors=[Range (Coord 0 0) (Coord 0 1)]
}

-- | Adjusts input ranges to contain at least one character.
ensureSize :: CrdRange -> CrdRange
ensureSize r@(Range start end)
  | start == end = 
    if start^.coordCol == 0 then r & rEnd %~ moveCursorByN 1
                          else r & rStart %~ moveCursorByN (-1)
  | otherwise = r

-- | Sorts Ranges, removes duplicates, ensures they contain at least one character
-- and restricts them to fit within the given text.
cleanRanges :: Y.YiString -> [CrdRange] -> [CrdRange]
cleanRanges txt = fmap (ensureSize . clampRange txt) . reverse . nub . sort

-- | A lens over all the stored cursor ranges for a buffer
getRanges :: BufAction [CrdRange]
getRanges = use (bufExt.cursors)

setRanges :: [CrdRange] -> BufAction ()
setRanges new = do
  txt <- getText
  bufExt.cursors .= cleanRanges txt new

overRanges :: ([CrdRange] -> [CrdRange]) -> BufAction ()
overRanges f = getRanges >>= setRanges . f

-- | Sequences actions over each range as a 'BufAction'
rangeDo :: (CrdRange -> BufAction a) -> BufAction [a]
rangeDo f = getRanges >>= mapM f

-- | 'rangeDo' with void return.
rangeDo_ :: (CrdRange -> BufAction a) -> BufAction ()
rangeDo_ = void . rangeDo

-- | Sequences actions over each range and replaces each range with its result.
overEachRange :: (CrdRange -> BufAction CrdRange) -> BufAction ()
overEachRange f = rangeDo f >>= setRanges

-- | Adds a new range to the list of ranges.
addRange :: CrdRange -> BufAction ()
addRange r = overRanges (++[r])

-- | Sets style attributes to show a given range.
displayRange ::  BufAction ()
displayRange = rangeDo_ setStyle
  where
    setStyle :: CrdRange -> BufAction ()
    setStyle r = addStyle r (flair ReverseVideo)
