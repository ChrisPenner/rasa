{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors.Internal.Base
  ( rangeDo
  , rangeDo_
  , ranges
  , eachRange
  , overRanges
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
  | start == end = r & rEnd %~ moveCursorByN 1
  | otherwise = r

-- | Sorts Ranges, removes duplicates, ensures they contain at least one character
-- and restricts them to fit within the given text.
cleanRanges :: Y.YiString -> [CrdRange] -> [CrdRange]
cleanRanges txt = fmap (ensureSize . clampRange txt) . reverse . nub . sort

-- | A lens over all the stored cursor ranges for a buffer
ranges :: HasBuffer s => Lens' s [CrdRange]
ranges = lens getter setter
  where getter buf = buf^.bufExt.cursors
        setter buf new = let txt = buf^.text
                          in buf & bufExt.cursors .~ cleanRanges txt new

-- | A Traversal over each Range for the given buffer.
eachRange :: HasBuffer s => Traversal' s CrdRange
eachRange = ranges.traverse

-- | Sequences actions over each range as a 'BufAction'
rangeDo :: (CrdRange -> BufAction a) -> BufAction [a]
rangeDo f = use ranges >>= mapM f

-- | 'rangeDo' with void return.
rangeDo_ :: (CrdRange -> BufAction a) -> BufAction ()
rangeDo_ = void . rangeDo

-- | Sequences actions over each range and replaces each range with its result.
overRanges :: (CrdRange -> BufAction CrdRange) -> BufAction ()
overRanges f = ranges <~ rangeDo f

-- | Adds a new range to the list of ranges.
addRange :: CrdRange -> BufAction ()
addRange r = ranges <>= [r]

-- | Sets style attributes to show a given range.
displayRange ::  BufAction ()
displayRange = rangeDo_ setStyle
  where
    setStyle :: CrdRange -> BufAction ()
    setStyle r = addStyle r (flair ReverseVideo)
