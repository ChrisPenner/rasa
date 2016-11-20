module TextLensTest where

import Test.Hspec
import TextLens as TL
import Control.Lens

textlens :: IO ()
textlens = hspec $ describe "TextLens" $ do
    describe "before" $ do
        it "gets before the offset" $
            "hello" ^. TL.before 3 `shouldBe` "hel"
        it "gets \"\" if before 0" $
            "hello" ^. TL.before 0 `shouldBe` ""
    describe "after" $ do
        it "gets after the offset" $
            "hello" ^. TL.after 3 `shouldBe` "lo"
        it "gets full string if after 0" $
            "hello" ^. TL.after 0 `shouldBe` "hello"
        it "gets \"\" if after end" $
            "hello" ^. TL.after 5 `shouldBe` ""
