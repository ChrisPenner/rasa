module Rasa.Ext.Cursors.Actions
  ( deleteChar
  , insertText
  , findNext
  , findPrev
  , findNextAt
  , findPrevAt
  ) where

import qualified Data.Text as T

import Control.Lens
import Control.Lens.Text
import Rasa.Ext
import Rasa.Ext.Cursors.Types
import Rasa.Ext.Directive
import Rasa.Ext.Cursors.Base

deleteChar :: BufAction ()
deleteChar = offsetsDo_ deleteCharAt

insertText :: T.Text -> BufAction ()
insertText txt = offsetsDo_ $ insertTextAt txt

findNext :: T.Text -> BufAction ()
findNext txt = moveOffsetsTo (findNextAt txt)

findNextAt :: T.Text -> Offset -> BufAction Offset
findNextAt txt o = (o+) <$> use (text . after o . tillNext txt . to T.length)

findPrev :: T.Text -> BufAction ()
findPrev txt = moveOffsetsTo (findPrevAt txt)

findPrevAt :: T.Text -> Offset -> BufAction Offset
findPrevAt txt o = (o+) <$> use (text . before o . tillPrev txt . to T.length . to negate)
