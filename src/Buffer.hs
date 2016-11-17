{-# LANGUAGE Rank2Types #-}

module Buffer (
    withCursor
  , inBuf
              ) where

import Data.Text as T
import Control.Monad.State (evalState, execState)

import TextLens
import State
import Control.Lens

withCursor :: (Int -> Lens' T.Text T.Text) -> Lens' Buffer T.Text
withCursor l = lens getter setter
    where getter = evalState $ do
            curs <- use cursor
            use (text.l curs)

          setter old new = flip execState old $ do
              curs <- use cursor
              (text.l curs) .= new

inBuf :: Lens' T.Text T.Text -> Lens' Buffer T.Text
inBuf = (text.)
