module Rasa.Internal.TextSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()
import ArbitraryInstances ()

import Rasa.Internal.Text

import qualified Yi.Rope as Y
import Control.Lens

spec :: Spec
spec = do
  describe "asText" $ do
    prop "view asText ~= Y.toText" $ \t -> t^.asText `shouldBe` Y.toText t
    prop "view (from asText) ~= Y.fromText" $ \t -> t^. from asText `shouldBe` Y.fromText t

  describe "asString" $ do
    prop "view asString ~= Y.toString" $ \t -> t^. asString `shouldBe` Y.toString t
    prop "view (from asString) ~= Y.fromString" $ \t -> t^. from asString `shouldBe` Y.fromString t

  describe "asLines" $ do
    prop "view asLines ~= Y.concat" $ \t -> t^.asLines `shouldBe` Y.lines' t
    prop "view (from asLines) ~= Y.fromLines" $ \t -> t^. from asLines `shouldBe` Y.concat t
