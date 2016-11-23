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
intillNextN n pat = lens getter setter
    where focus :: Traversal' T.Text T.Text
          focus = splittingByInc pat . moreThanOne . lTake n . joiningByInc pat
          getter = view focus
          setter old new = old & focus .~ new

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
          focus = splittingBy pat . moreThanOne . lTake n . joiningBy pat
          getter = view focus
          setter old new = old & focus .~ new


tillPrevN :: Int -> T.Text -> Lens' T.Text T.Text
tillPrevN n pat = lens getter setter
    where focus :: Traversal' T.Text T.Text
          focus =
              splittingBy pat .
              reversed .
              lTake n .
              reversed .
              joiningBy pat

          getter = view focus
          setter old new = old & focus .~ new

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


moreThanOne :: Prism' [a] [a]
moreThanOne = prism' review' getter
    where review' = id
          getter :: [a] -> Maybe [a]
          getter [] = Nothing
          getter [_] = Nothing
          getter xs = Just xs


splittingBy :: T.Text -> Lens' T.Text  [T.Text]
splittingBy pat = lens getter setter
    where getter = T.splitOn pat
          setter _ = T.intercalate pat

splittingByInc :: T.Text -> Lens' T.Text  [T.Text]
splittingByInc pat = lens getter setter
    where getter txt = let lst = T.splitOn pat txt
                        in lst & reversed . dropping 1 traverse %~ (<> pat)
          setter _ = T.concat

-- splittingBy :: T.Text -> Prism' T.Text  [T.Text]
-- splittingBy pat = prism' review' getter
--     where review' = T.intercalate pat
--           getter :: T.Text -> Maybe [T.Text]
--           getter t = case T.splitOn pat t of
--                    [_] -> Nothing
--                    xs -> Just xs

joiningByInc :: T.Text -> Lens' [T.Text] T.Text
joiningByInc pat = lens getter setter
    where getter = T.concat
          setter _ new = T.splitOn pat new & reversed . dropping 1 traversed %~ (<> pat)

joiningBy :: T.Text -> Lens' [T.Text] T.Text
joiningBy pat = lens getter setter
    where getter = T.intercalate pat
          setter _ = T.splitOn pat

lTake :: Int -> Lens' [a] [a]
lTake n = lens getter setter
    where getter = take n
          setter old new = new ++ drop n old

lDrop :: Int -> Lens' [a] [a]
lDrop n = lens getter setter
    where getter = drop n
          setter old new = take n old ++ new
