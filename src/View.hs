{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module View (
    textWrap
  , Size
  , Renderable
  , render
    )
    where

import Types

import qualified Data.Text as T
import Data.List (unfoldr)
import Control.Arrow (second)

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
