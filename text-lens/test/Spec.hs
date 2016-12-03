import Test.Hspec
import Control.Lens.Text as TL
import Control.Lens

main :: IO ()
main = hspec $ describe "TextLens" $ do

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

    describe "splittingBy" $ do
        it "gets the split list" $
            "hi|there|you" ^. splittingBy "|" `shouldBe` ["hi", "there", "you"]

        it "sets splits" $
            ("hi|there|you" & splittingBy "|" .~ ["new", "stuff"]) `shouldBe` "new|stuff"

    describe "joiningBy" $ do
        it "gets the joined list" $
            ["hi", "there", "you"] ^. joiningBy "|" `shouldBe` "hi|there|you"
        it "gets the joined elem" $
            ["hi"] ^. joiningBy "|" `shouldBe` "hi"

        it "sets the joined list" $
            (["hi", "there"] & joiningBy "|" .~ "new|stuff") `shouldBe` ["new", "stuff"]

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

    describe "tillNextN" $ do
        it "gets text up to but not including next pattern" $
            "my test str" ^. TL.tillNextN 1 "te" `shouldBe` "my "
        it "gets text up to but not including 2nd pattern" $
            "my test str test end" ^. TL.tillNextN 2 "test" `shouldBe` "my test str "
        it "gets \"\" when pattern is first" $
            "my test" ^. TL.tillNextN 1 "my" `shouldBe` ""
        it "gets \"\" when no match" $
            "my test" ^. TL.tillNextN 1 "nope" `shouldBe` ""

        it "sets text up to but not including next pattern" $
            ("my test test str" & TL.tillNextN 2 "te" .~ "_") `shouldBe` "_test str"
        it "set is null op if no match found" $
            ("my test str" & TL.tillNextN 1 "xx" .~ "_") `shouldBe` "my test str"


    describe "intillPrevN" $ do
        it "gets text up to and including prev pattern" $
            "my test str" ^. TL.intillPrevN 1 "test" `shouldBe` "test str"
        it "gets text up to and including 2nd pattern" $
            "my test str test end" ^. TL.intillPrevN 2 "test" `shouldBe` "test str test end"
        it "gets when pattern is first" $
            "my test" ^. TL.intillPrevN 1 "st" `shouldBe` "st"
        it "gets \"\" when no match" $
            "my test" ^. TL.intillPrevN 1 "nope" `shouldBe` ""

        it "sets text up to and including prev pattern" $
            ("my test test str" & TL.intillPrevN 2 "te" .~ "_") `shouldBe` "my _"
        it "set is null op if no match found" $
            ("my test str" & TL.intillPrevN 1 "xx" .~ "_") `shouldBe` "my test str"


    describe "tillPrevN" $ do
        it "gets text up to but not including prev pattern" $
            "my test str" ^. TL.tillPrevN 1 "test" `shouldBe` " str"
        it "gets text up to but not including 2nd pattern" $
            "my test str test end" ^. TL.tillPrevN 2 "test" `shouldBe` " str test end"
        it "gets \"\" when pattern is first" $
            "my test" ^. TL.tillPrevN 1 "test" `shouldBe` ""
        it "gets \"\" when no match" $
            "my test" ^. TL.tillPrevN 1 "nope" `shouldBe` ""


        it "sets text up to but not including next pattern" $
            ("my test test str" & TL.tillPrevN 2 "te" .~ "_") `shouldBe` "my te_"
        it "set is null op if no match found" $
            ("my test str" & TL.tillPrevN 1 "xx" .~ "_") `shouldBe` "my test str"

    describe "range" $ do
        it "gets the proper range" $
            "hi hey hello" ^. TL.range 3 6 `shouldBe` "hey"
        it "gets \"\" if invalid range" $
            "hello" ^. TL.range 5 3 `shouldBe` ""
        it "gets full string" $
            "hello" ^. TL.range 0 5 `shouldBe` "hello"

        it "sets the range" $
            ("hi hey hello" & TL.range 3 6 .~ "__") `shouldBe` "hi __ hello"
        it "sets empty range" $
            ("hi hey hello" & TL.range 3 3 .~ "greetings ") `shouldBe` "hi greetings hey hello"
        it "set is null op if invalid range" $
            ("hi hey hello" & TL.range 7 3 .~ "__") `shouldBe` "hi hey hello"

    describe "matching" $ do
        it "gets matches" $
            "hi hey hello" ^. TL.matching "he" `shouldBe` "hehe"
        it "gets \"\" if no match" $
            "hello" ^. TL.matching "z" `shouldBe` ""

        it "sets matches" $
            ("hi hey hello" & TL.matching "he" .~ "__") `shouldBe` "hi __y __llo"
        it "set is null op if no match" $
            ("hi hey hello" & TL.matching "z" .~ "__") `shouldBe` "hi hey hello"
