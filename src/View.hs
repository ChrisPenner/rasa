{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module View (
    textWrap
  , Size
  , Renderable
  , render
    )
    where

import qualified Data.Text as T
import Data.List.Extra (takeEnd)
import Data.List (unfoldr)
import Control.Lens
import Control.Arrow (second)

import State

type Size = (Int, Int)

class Renderable a b where
    render :: Size -> a -> b

textWrap :: Int -> T.Text -> T.Text
textWrap n = T.dropEnd 1 . T.unlines . unfoldr (splitLine n)

splitLine :: Int -> (T.Text -> Maybe (T.Text, T.Text))
splitLine n t
  | T.null t = Nothing
  | T.compareLength (fst . splitAtNewline $ t) n == LT = Just $ splitAtNewline t
  | otherwise = Just $ second (T.append "-> ") $ T.splitAt n t

splitAtNewline :: T.Text -> (T.Text, T.Text)
splitAtNewline = second (T.drop 1) . T.span (/= '\n')
