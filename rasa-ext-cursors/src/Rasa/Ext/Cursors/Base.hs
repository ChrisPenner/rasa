{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors.Base
  ( rangeDo
  , rangeDo_
  , ranges
  , displayRange
  , eachRange
  , overRanges
  , addRange
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
  _cursors=[Range (Coord 0 0) (Coord 0 1)]
}

cleanRanges :: Y.YiString -> [Range] -> [Range]
cleanRanges txt = fmap (clampRange txt) . reverse . nub . sort

ranges :: Lens' Buffer [Range]
ranges = lens getter setter
  where getter buf = buf^.bufExt.cursors
        setter buf new = let txt = buf^.rope
                          in buf & bufExt.cursors .~ cleanRanges txt new

eachRange :: Traversal' Buffer Range
eachRange = ranges.traverse

rangeDo :: (Range -> BufAction a) -> BufAction [a]
rangeDo f = use ranges >>= mapM f

rangeDo_ :: (Range -> BufAction a) -> BufAction ()
rangeDo_ = void . rangeDo

overRanges :: (Range -> BufAction Range) -> BufAction ()
overRanges f = ranges <~ rangeDo f

addRange :: Range -> BufAction ()
addRange r = ranges <>= [r]

displayRange ::  BufAction ()
displayRange = rangeDo_ setStyle
  where
    setStyle :: Range -> BufAction ()
    setStyle r = addStyle r (flair ReverseVideo)
