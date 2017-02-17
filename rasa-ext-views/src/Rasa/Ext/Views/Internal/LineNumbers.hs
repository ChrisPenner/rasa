module Rasa.Ext.Views.Internal.LineNumbers
  ( lineNumbers
  , enableLineNumbers
  , disableLineNumbers
  , toggleLineNumbers
  , checkLineNumbers
  ) where

import Rasa.Ext

import Rasa.Ext.Views.Internal.Widgets
import Data.Default
import qualified Yi.Rope as Y

newtype LineNumbers =
  LineNumbers Bool
  deriving Show

instance Default LineNumbers where
  def = LineNumbers True

enableLineNumbers :: BufAction ()
enableLineNumbers = setBufExt $ LineNumbers True

disableLineNumbers :: BufAction ()
disableLineNumbers = setBufExt $ LineNumbers False

toggleLineNumbers :: BufAction ()
toggleLineNumbers = overBufExt $
  \(LineNumbers b) -> LineNumbers $ not b

checkLineNumbers :: BufAction Bool
checkLineNumbers = do
  LineNumbers b <- getBufExt
  return b

lineNumbers :: Action ()
lineNumbers = onEveryNewBuffer_ . addLeftBar $ do
  enabled <- checkLineNumbers
  if enabled
     then Just <$> getLineNumbers
     else return Nothing
  where
    getLineNumbers = do
      numLines <- Y.countNewLines <$> getText
      return . Y.unlines $ Y.fromString . show <$> [1.. numLines + 1]
