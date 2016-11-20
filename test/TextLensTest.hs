module TextLensTest where

import Test.Hspec
import TextLens as TL
import Control.Lens

textlens :: IO ()
textlens = hspec $ describe "TextLens" $ do
    describe "before" $ do
        it "gets before the offset" $
            "hello" ^. TL.before 3 `shouldBe` "hel"
        it "gets full string if before end" $
            "hello" ^. TL.before 5 `shouldBe` "hello"
        it "gets \"\" if before 0" $
            "hello" ^. TL.before 0 `shouldBe` ""
        it "gets full string if out of bounds" $
            "hello" ^. TL.before 10 `shouldBe` "hello"

        it "sets before the offset" $
            ("hello" & TL.before 3 .~ "zz") `shouldBe` "zzlo"
        it "prepends if before 0" $
            ("hello" & TL.before 0 .~ "zz") `shouldBe` "zzhello"

    describe "after" $ do
        it "gets after the offset" $
            "hello" ^. TL.after 3 `shouldBe` "lo"
        it "gets full string if after 0" $
            "hello" ^. TL.after 0 `shouldBe` "hello"
        it "gets \"\" if at end" $
            "hello" ^. TL.after 5 `shouldBe` ""
        it "gets \"\" if out of bounds end" $
            "hello" ^. TL.after 10 `shouldBe` ""

        it "sets after the offset" $
            ("hello" & TL.after 3 .~ "zz") `shouldBe` "helzz"
        it "appends if before end" $
            ("hello" & TL.after 5 .~ "zz") `shouldBe` "hellozz"


    describe "intillNextN" $ do
        it "gets text up to and including next pattern" $
            "my test str" ^. TL.intillNextN 1 "te" `shouldBe` "my te"
        it "gets text up to and including 2nd pattern" $
            "my test str test end" ^. TL.intillNextN 2 "test" `shouldBe` "my test str test"
        it "gets when pattern is first" $
            "my test" ^. TL.intillNextN 1 "my" `shouldBe` "my"
        it "gets \"\" when no match" $
            "my test" ^. TL.intillNextN 1 "nope" `shouldBe` ""

        it "sets text up to and including next pattern" $
            ("my test test str" & TL.intillNextN 2 "te" .~ "_") `shouldBe` "_st str"

        it "set is null op if no match found" $
            ("my test str" & TL.intillNextN 1 "xx" .~ "_") `shouldBe` "my test str"
