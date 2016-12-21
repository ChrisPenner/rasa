{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors.Base
  ( coordsDo
  , coordsDo_
  , offsetsDo
  , offsetsDo_
  , displayCursor
  , offsets
  , coords
  , eachCoord
  , eachOffset
  , addCursorCoordAt
  , addCursorOffsetAt
  ) where


import Rasa.Ext
import Rasa.Ext.Scheduler
import Rasa.Ext.Directive
import Rasa.Ext.Style

import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.List
import Data.Default
import qualified Yi.Rope as Y

import Rasa.Ext.Cursors.Types

newtype Cursor = Cursor
  { _cursors :: [Coord]
  } deriving (Show, Typeable)

makeLenses ''Cursor

instance Default Cursor where
  def = Cursor {
  _cursors=[Coord 0 0]
}

cleanCursors :: Y.YiString -> [Coord] -> [Coord]
cleanCursors txt = fmap (clampCoord txt) . reverse . nub . sort

coords :: Lens' Buffer [Coord]
coords = lens getter setter
  where getter buf = buf^.bufExt.cursors
        setter buf new = let txt = buf^.rope
                          in buf & bufExt.cursors .~ cleanCursors txt new


offsets :: Lens' Buffer [Offset]
offsets = lens getter setter
  where getter buf = let txt = buf^.rope
                      in buf^..bufExt.cursors.to sort.to nub.reversed.traverse.from (asCoord txt)
        setter buf new = let txt = buf^.rope
                          in buf & bufExt.cursors .~ cleanCursors txt (view (asCoord txt) <$> new)

eachCoord :: Traversal' Buffer Coord
eachCoord = coords.traverse

eachOffset :: Traversal' Buffer Offset
eachOffset = offsets.traverse

offsetsDo :: (Offset -> BufAction a) -> BufAction [a]
offsetsDo f = use offsets >>= mapM f

offsetsDo_ :: (Offset -> BufAction a) -> BufAction ()
offsetsDo_ = void . offsetsDo

coordsDo :: (Coord -> BufAction a) -> BufAction [a]
coordsDo f = use coords >>= mapM f

coordsDo_ :: (Coord -> BufAction a) -> BufAction ()
coordsDo_ = void . coordsDo

addCursorCoordAt :: Coord -> BufAction ()
addCursorCoordAt c = coords %= (c:)

addCursorOffsetAt :: Int -> BufAction ()
addCursorOffsetAt o = offsets %= (o:)

displayCursor ::  BufAction ()
displayCursor = offsetsDo_ setStyle
  where
    setStyle :: Offset -> BufAction ()
    setStyle o = addStyle $ Span o (o+1) (flair ReverseVideo)
