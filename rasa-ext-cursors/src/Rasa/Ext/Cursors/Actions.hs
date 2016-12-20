module Rasa.Ext.Cursors.Actions
  ( deleteChar
  , insertText
  , findNext
  , findPrev
  ) where

import qualified Data.Text as T

import Control.Lens
import Control.Lens.Text
import Rasa.Ext
import Rasa.Ext.Directive
import Rasa.Ext.Cursors.Base

deleteChar :: BufAction ()
deleteChar = offsetsDo_ deleteCharAt

insertText :: T.Text -> BufAction ()
insertText txt = offsetsDo_ $ insertTextAt txt

findNext :: T.Text -> BufAction ()
findNext txt = moveOffsetsBy distNext
  where
    distNext :: Int -> BufAction Int
    distNext o = use (text . after o . tillNext txt . to T.length)

findPrev :: T.Text -> BufAction ()
findPrev txt = moveOffsetsBy distPrev
  where
    distPrev :: Int -> BufAction Int
    distPrev o = use (text . before o . tillPrev txt . to T.length . to negate)
