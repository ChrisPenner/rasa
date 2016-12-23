{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors.Base
  ( --coordsDo
  -- , coordsDo_
  -- , offsetsDo
  -- , offsetsDo_
  rangeDo
  , rangeDo_
  , ranges
  , displayRange
  , eachRange
  , moveRanges
  -- , offsets
  -- , coords
  -- , eachCoord
  -- , eachOffset
  -- , addCursorCoordAt
  -- , addCursorOffsetAt
  ) where


import Rasa.Ext
import Rasa.Ext.Style

import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.List
import Data.Default
import qualified Yi.Rope as Y

data Cursors = Cursors
  { _cursors :: [Range]
  } deriving (Typeable, Show)

makeLenses ''Cursors

instance Default Cursors where
  def = Cursors {
  _cursors=[Range (Right $ Coord 0 0) (Right $ Coord 0 1)]
}

cleanRanges :: Y.YiString -> [Range] -> [Range]
cleanRanges txt = fmap (clampRange txt) . reverse . nub . sortOn getEnd
  where
    getEnd (Range _ end) = let Offset o = cursorToOffset txt end
                            in o

ranges :: Lens' Buffer [Range]
ranges = lens getter setter
  where getter buf = buf^.bufExt.cursors
        setter buf new = let txt = buf^.rope
                          in buf & bufExt.cursors .~ cleanRanges txt new


-- coords :: Lens' Buffer Cursors
-- coords = lens getter setter
--   where getter buf = buf^.bufExt.cursors
--         setter buf new = let txt = buf^.rope
--                           in buf & bufExt.cursors .~ cleanCursors txt new


-- offsets :: Lens' Buffer [Offset]
-- offsets = lens getter setter
--   where getter buf = let txt = buf^.rope
--                       in buf^..bufExt.cursors.to sort.to nub.reversed.traverse.from (asCoord txt)
--         setter buf new = let txt = buf^.rope
--                           in buf & bufExt.cursors .~ cleanCursors txt (view (asCoord txt) <$> new)

eachRange :: Traversal' Buffer Range
eachRange = ranges.traverse

-- eachCoord :: Traversal' Buffer Coord
-- eachCoord = coords.traverse

-- eachOffset :: Traversal' Buffer Offset
-- eachOffset = offsets.traverse

rangeDo :: (Range -> BufAction a) -> BufAction [a]
rangeDo f = use ranges >>= mapM f

rangeDo_ :: (Range -> BufAction a) -> BufAction ()
rangeDo_ = void . rangeDo

-- offsetsDo :: (Offset -> BufAction a) -> BufAction [a]
-- offsetsDo f = use offsets >>= mapM f

-- offsetsDo_ :: (Offset -> BufAction a) -> BufAction ()
-- offsetsDo_ = void . offsetsDo

-- coordsDo :: (Coord -> BufAction a) -> BufAction [a]
-- coordsDo f = use coords >>= mapM f

-- coordsDo_ :: (Coord -> BufAction a) -> BufAction ()
-- coordsDo_ = void . coordsDo

-- addCursorCoordAt :: Coord -> BufAction ()
-- addCursorCoordAt c = coords %= (c:)

-- addCursorOffsetAt :: Int -> BufAction ()
-- addCursorOffsetAt o = offsets %= (o:)

-- displayCursor ::  BufAction ()
-- displayCursor = offsetsDo_ setStyle
--   where
--     setStyle :: Offset -> BufAction ()
--     setStyle o = addStyle $ Span o (o+1) (flair ReverseVideo)

displayRange ::  BufAction ()
displayRange = rangeDo_ setStyle
  where
    setStyle :: Range -> BufAction ()
    setStyle r = addStyle r (flair ReverseVideo)

moveRanges :: Cursor -> BufAction ()
moveRanges n = do
  txt <- use rope
  eachRange %= moveRange txt n
