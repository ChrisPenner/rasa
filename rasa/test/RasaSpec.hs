module RasaSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Rasa" $ do
    it "runs tests" $
      True `shouldBe` True
