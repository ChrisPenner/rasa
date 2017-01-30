{-# language ScopedTypeVariables, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryInstances where

import qualified Data.Text as T
import qualified Yi.Rope as Y

import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Rasa.Internal.Range

instance Arbitrary Y.YiString where
  arbitrary = do
    txt <- arbitrary :: Gen T.Text
    return $ Y.fromText txt

instance Arbitrary (Coord' Int Int) where
  arbitrary = do
    (r, c) <- arbitrary :: Gen (Int, Int)
    return $ Coord (abs r) (abs c)
