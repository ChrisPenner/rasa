{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module TextLens where

import Control.Lens
import qualified Data.Text as T
import Data.Monoid
import Data.List.Extra (takeEnd, dropEnd)


before :: Int -> Lens' T.Text T.Text
before n = lens getter setter
    where getter = T.take n
          setter old new = new <> T.drop n old

after :: Int -> Lens' T.Text T.Text
after n = lens getter setter
    where getter = T.drop n
          setter old new = T.take n old <> new


intillNextN :: Int -> T.Text -> Lens' T.Text T.Text
intillNextN 0 _ = lens (const "") const
intillNextN n pat = lens getter setter
    where getter = pad . T.intercalate pat . take n . split' pat
          setter old new =
              T.append new . T.intercalate pat . drop n . split' pat $ old
          pad = (<> pat)

intillPrevN :: Int -> T.Text -> Lens' T.Text T.Text
intillPrevN 0 _ = lens (const "") const
intillPrevN n pat = lens getter setter
    where getter = padIfFound . T.intercalate pat . takeEnd n . split' pat
          setter old new =
              (<> new) . T.intercalate pat . dropEnd n . split' pat $ old
          padIfFound x = pat <> x

tillNextN :: Int -> T.Text -> Lens' T.Text T.Text
tillNextN n pat = lens getter setter
    where focus :: Traversal' T.Text T.Text
          focus = splittingBy pat . partsOf (taking n each) . joiningBy pat
          getter = view focus
          setter old new = let len' = T.length <$> old ^? focus
                            in case len' of
                                 Nothing -> old
                                 Just len -> set (before len) new old


tillPrevN :: Int -> T.Text -> Lens' T.Text T.Text
tillPrevN n pat = lens getter setter
    where focus :: Traversal' T.Text T.Text
          focus =
              splittingBy pat .
              reversed .
              partsOf(taking n each) .
              reversed .
              joiningBy pat

          getter = view focus
          setter old new = let len' = T.length <$> old ^? focus
                            in case len' of
                                 Nothing -> old
                                 Just len -> set (after len) new old

tillNext :: T.Text -> Lens' T.Text T.Text
tillNext = tillNextN 1

intillNext :: T.Text -> Lens' T.Text T.Text
intillNext = intillNextN 1

tillPrev :: T.Text -> Lens' T.Text T.Text
tillPrev = tillPrevN 1

intillPrev :: T.Text -> Lens' T.Text T.Text
intillPrev = intillPrevN 1

range :: Int -> Int -> Lens' T.Text T.Text
range start end = lens getter setter
    where getter = T.take (end - start) . T.drop start
          setter old new
              | start > end = old
              | otherwise = T.take start old <> new <> T.drop end old

matching :: T.Text -> Lens' T.Text T.Text
matching pat = lens getter setter
    where getter = (`T.replicate` pat) . T.count pat
          setter old new = T.replace pat new old

split' :: T.Text -> T.Text -> [T.Text]
split' pat t = case T.splitOn pat t of
                [_] -> []
                xs -> xs

splittingBy :: T.Text -> Prism' T.Text  [T.Text]
splittingBy pat = prism' review' getter
    where review' = T.intercalate pat
          getter :: T.Text -> Maybe [T.Text]
          getter t = case T.splitOn pat t of
                   [_] -> Nothing
                   xs -> Just xs

joiningBy :: T.Text -> Lens' [T.Text] T.Text
joiningBy pat = lens getter setter
    where getter = T.intercalate pat
          setter _ = T.splitOn pat
