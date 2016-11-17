{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module TextLens (
    before
  , after
  , tillNext
  , tillPrev
  , reversed
  , range
    )
    where

import Control.Lens
import Data.Text as T
import Data.Monoid


before :: Int -> Lens' T.Text T.Text
before n = lens getter (flip setter)
    where getter = T.take n
          setter old new = T.drop n old <> new

after :: Int -> Lens' T.Text T.Text
after n = lens getter setter
    where getter = T.drop n
          setter old new = T.take n old <> new

tillNext :: T.Text -> Lens' T.Text T.Text
tillNext pat = lens getter setter
    where getter = fst . split
          setter old new = new <> snd (split old)
          split = T.breakOn pat

tillPrev :: T.Text -> Lens' T.Text T.Text
tillPrev pat = lens getter setter
    where getter = snd . split
          setter old new = fst (split old) <> new
          split = T.breakOnEnd pat

range :: Int -> Int -> Lens' T.Text T.Text
range start end = lens getter setter
    where getter = T.take (end - start) . T.drop start
          setter old new = T.take start old <> new <> T.drop end old
