module Rasa.Ext.Cursors.Actions
  ( deleteChar
  , insertText
  , findNext
  , findPrev
  , findOffsetNext
  , findOffsetPrev
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
findNext txt = offsets <~ offsetsDo (findOffsetNext txt)

findOffsetNext :: T.Text -> Offset -> BufAction Offset
findOffsetNext txt o = (o+) <$> use (text . after o . tillNext txt . to T.length)

findPrev :: T.Text -> BufAction ()
findPrev txt = offsets <~ offsetsDo (findOffsetPrev txt)

findOffsetPrev :: T.Text -> Offset -> BufAction Offset
findOffsetPrev txt o = (o+) <$> use (text . before o . tillPrev txt . to T.length . to negate)
