module Rasa.Internal.RangeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()
import ArbitraryInstances ()

import Rasa.Internal.Range

spec :: Spec
spec = do
  describe "overRow" $
    prop "overRow f ~= Coord (f a) b" $ \crd@(Coord r c) -> Coord (r+1) c `shouldBe` overRow (+1) crd

  describe "overCol" $
    prop "overCol f ~= Coord a (f b)" $ \crd@(Coord r c) -> Coord r (c+1) `shouldBe` overCol (+1) crd

  describe "overBoth" $
    prop "overBoth f ~= Coord (f a) (f b)" $ \crd@(Coord r c) -> Coord (r+1) (c+1) `shouldBe` overBoth (+ (1 :: Int)) crd
