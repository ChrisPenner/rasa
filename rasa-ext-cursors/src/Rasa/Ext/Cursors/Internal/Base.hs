{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors.Internal.Base
  ( rangeDo
  , rangeDo_
  , overRanges
  , getRanges
  , setRanges
  , overEachRange
  , addRange
  , setStyleProvider
  ) where


import Rasa.Ext

import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.List
import Data.Default
import qualified Yi.Rope as Y

-- | Stores the cursor ranges in each buffer.
data Cursors =
  Cursors [CrdRange]
  deriving (Typeable, Show)

instance Default Cursors where
  def = Cursors [Range (Coord 0 0) (Coord 0 1)]

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

-- | Get the list of ranges
getRanges :: BufAction [CrdRange]
getRanges = do
  Cursors ranges <- getBufExt
  return ranges

setRanges :: [CrdRange] -> BufAction ()
setRanges new = do
  txt <- getText
  setBufExt . Cursors $ cleanRanges txt new

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

-- | Adds cursor specific styles
setStyleProvider :: BufAction ()
setStyleProvider = void . addStyleProvider $ rangeDo setStyle
  where
    setStyle :: CrdRange -> BufAction (Span CrdRange Style)
    setStyle r = return $ Span r (flair ReverseVideo)
